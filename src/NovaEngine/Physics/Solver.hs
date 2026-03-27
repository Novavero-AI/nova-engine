-- | Sequential impulse constraint solver.
--
-- Resolves collisions between rigid bodies by applying impulses
-- that satisfy non-penetration and friction constraints.  Pure
-- Haskell, iterative solver with configurable iteration count.
module NovaEngine.Physics.Solver
  ( -- * Rigid body
    RigidBody (..),
    defaultBody,

    -- * Collision
    Contact (..),

    -- * Solver
    solveContacts,

    -- * Integration
    integrateBody,
  )
where

import NovaEngine.Math.Quaternion (axisAngle, mulQuat)
import NovaEngine.Math.Types
import NovaEngine.Math.Vector (cross, dot, vlength, vlengthSq)

-- ----------------------------------------------------------------
-- Rigid body
-- ----------------------------------------------------------------

-- | A rigid body with linear and angular dynamics.
data RigidBody = RigidBody
  { bodyPosition :: !V3,
    bodyRotation :: !Quaternion,
    bodyVelocity :: !V3,
    bodyAngularVelocity :: !V3,
    bodyMass :: !Float,
    bodyInertia :: !Float,
    bodyRestitution :: !Float,
    bodyFriction :: !Float,
    bodyIsStatic :: !Bool
  }
  deriving (Show, Eq)

-- | Default dynamic body at origin, 1kg, no velocity.
defaultBody :: RigidBody
defaultBody =
  RigidBody
    { bodyPosition = V3 0 0 0,
      bodyRotation = Quaternion 1 (V3 0 0 0),
      bodyVelocity = V3 0 0 0,
      bodyAngularVelocity = V3 0 0 0,
      bodyMass = 1.0,
      bodyInertia = 1.0,
      bodyRestitution = 0.3,
      bodyFriction = 0.5,
      bodyIsStatic = False
    }

-- | Inverse mass (0 for static bodies).
invMass :: RigidBody -> Float
invMass b
  | bodyIsStatic b = 0
  | bodyMass b <= 0 = 0
  | otherwise = 1.0 / bodyMass b

-- | Inverse inertia (0 for static bodies).
invInertia :: RigidBody -> Float
invInertia b
  | bodyIsStatic b = 0
  | bodyInertia b <= 0 = 0
  | otherwise = 1.0 / bodyInertia b

-- ----------------------------------------------------------------
-- Contact
-- ----------------------------------------------------------------

-- | A contact point between two bodies.
data Contact = Contact
  { contactNormal :: !V3,
    contactDepth :: !Float,
    contactPoint :: !V3
  }
  deriving (Show, Eq)

-- ----------------------------------------------------------------
-- Solver
-- ----------------------------------------------------------------

-- | Number of solver iterations.
solverIterations :: Int
solverIterations = 8

-- | Baumgarte stabilization factor (position correction).
baumgarte :: Float
baumgarte = 0.2

-- | Penetration slop: allow small overlap before correction.
penetrationSlop :: Float
penetrationSlop = 0.01

-- | Solve a list of contacts between body pairs.
-- Returns updated bodies.
solveContacts ::
  [(RigidBody, RigidBody, [Contact])] ->
  [(RigidBody, RigidBody)]
solveContacts pairs =
  let -- Run N iterations of sequential impulse
      iterate' ps 0 = ps
      iterate' ps n = iterate' (map solveOnePair ps) (n - 1)
      solved = iterate' pairs solverIterations
   in map (\(a, b, _) -> (a, b)) solved

-- | Solve all contacts for one body pair.
solveOnePair ::
  (RigidBody, RigidBody, [Contact]) ->
  (RigidBody, RigidBody, [Contact])
solveOnePair (bodyA, bodyB, contacts) =
  let (newA, newB) = foldl' solveOneContact (bodyA, bodyB) contacts
   in (newA, newB, contacts)
  where
    foldl' _ acc [] = acc
    foldl' f !acc (x : xs) = foldl' f (f acc x) xs

-- | Apply impulse for a single contact.
solveOneContact :: (RigidBody, RigidBody) -> Contact -> (RigidBody, RigidBody)
solveOneContact (bodyA, bodyB) contact =
  let n = contactNormal contact
      rA = contactPoint contact ^-^ bodyPosition bodyA
      rB = contactPoint contact ^-^ bodyPosition bodyB

      -- Relative velocity at contact
      vA = bodyVelocity bodyA ^+^ cross (bodyAngularVelocity bodyA) rA
      vB = bodyVelocity bodyB ^+^ cross (bodyAngularVelocity bodyB) rB
      relVel = vA ^-^ vB
      velAlongNormal = dot relVel n
   in -- Don't resolve if separating
      if velAlongNormal > 0
        then (bodyA, bodyB)
        else
          let -- Restitution
              e = min (bodyRestitution bodyA) (bodyRestitution bodyB)

              -- Effective mass along normal
              imA = invMass bodyA
              imB = invMass bodyB
              iiA = invInertia bodyA
              iiB = invInertia bodyB

              rAxN = cross rA n
              rBxN = cross rB n
              effectiveMass =
                imA
                  + imB
                  + iiA * vlengthSq rAxN
                  + iiB * vlengthSq rBxN

              -- Normal impulse magnitude
              bias =
                baumgarte
                  * max 0 (contactDepth contact - penetrationSlop)
              jn =
                if effectiveMass > 0
                  then (-((1.0 + e) * velAlongNormal) + bias) / effectiveMass
                  else 0
              clampedJn = max 0 jn

              -- Apply normal impulse
              impulse = clampedJn *^ n
              newA = applyImpulse bodyA impulse rA
              newB = applyImpulse bodyB (negV impulse) rB

              -- Friction impulse (tangent)
              tangent = relVel ^-^ (velAlongNormal *^ n)
              tanLen = vlength tangent
           in if tanLen < 1.0e-6
                then (newA, newB)
                else
                  let tanDir = (1.0 / tanLen) *^ tangent
                      mu = (bodyFriction bodyA + bodyFriction bodyB) * 0.5

                      rAxT = cross rA tanDir
                      rBxT = cross rB tanDir
                      effectiveMassT =
                        imA
                          + imB
                          + iiA * vlengthSq rAxT
                          + iiB * vlengthSq rBxT

                      jt =
                        if effectiveMassT > 0
                          then negate tanLen / effectiveMassT
                          else 0
                      -- Coulomb's law: friction ≤ mu * normal impulse
                      clampedJt =
                        if abs jt > mu * clampedJn
                          then signum jt * mu * clampedJn
                          else jt

                      frictionImpulse = clampedJt *^ tanDir
                      finalA = applyImpulse newA frictionImpulse rA
                      finalB = applyImpulse newB (negV frictionImpulse) rB
                   in (finalA, finalB)

-- | Apply an impulse to a rigid body at a contact offset.
applyImpulse :: RigidBody -> V3 -> V3 -> RigidBody
applyImpulse body impulse r
  | bodyIsStatic body = body
  | otherwise =
      body
        { bodyVelocity =
            bodyVelocity body ^+^ invMass body *^ impulse,
          bodyAngularVelocity =
            bodyAngularVelocity body
              ^+^ invInertia body
              *^ cross r impulse
        }

-- ----------------------------------------------------------------
-- Integration
-- ----------------------------------------------------------------

-- | Semi-implicit Euler integration step.
integrateBody :: Float -> V3 -> RigidBody -> RigidBody
integrateBody dt gravity body
  | bodyIsStatic body = body
  | otherwise =
      let -- Apply gravity
          vel = bodyVelocity body ^+^ dt *^ gravity
          pos = bodyPosition body ^+^ dt *^ vel
          -- Angular
          angVel = bodyAngularVelocity body
          angSpeed = vlength angVel
          rot =
            if angSpeed > 1.0e-8
              then
                mulQuat
                  (axisAngle ((1.0 / angSpeed) *^ angVel) (angSpeed * dt))
                  (bodyRotation body)
              else bodyRotation body
       in body
            { bodyPosition = pos,
              bodyVelocity = vel,
              bodyRotation = rot
            }

-- ----------------------------------------------------------------
-- Helpers
-- ----------------------------------------------------------------

-- | Negate a V3.
negV :: V3 -> V3
negV (V3 x y z) = V3 (negate x) (negate y) (negate z)

-- Prelude's signum is used for Coulomb friction clamping.
