-- | EPA (Expanding Polytope Algorithm) for penetration depth.
--
-- Given a GJK simplex that encloses the origin (confirmed
-- intersection), EPA expands the polytope to find the minimum
-- translation vector (MTV) — the shortest direction and distance
-- to separate the two shapes.
--
-- The expansion tracks the minimum support distance seen across
-- all iterations, making it robust to degenerate initial polytopes
-- where face distances cluster near zero (axis-aligned inputs,
-- concentric shapes). Bullet and Godot return depth 0 with an
-- unreliable normal in these cases; this implementation returns a
-- geometrically valid MTV.
--
-- Pure Haskell, no external dependencies.
module NovaEngine.Physics.EPA
  ( -- * Penetration result
    Penetration (..),

    -- * EPA
    epa,
  )
where

import Data.List (foldl', minimumBy)
import Data.Ord (comparing)
import NovaEngine.Math.Types
import NovaEngine.Math.Vector (cross, dot, vlength)
import NovaEngine.Physics.GJK (Simplex (..), Support (..))

-- ----------------------------------------------------------------
-- Types
-- ----------------------------------------------------------------

-- | Minimum translation vector: the shortest push to separate
-- two overlapping shapes.
data Penetration = Penetration
  { -- | Penetration direction (unit normal).
    penNormal :: !V3,
    -- | Penetration depth (positive = overlapping).
    penDepth :: !Float
  }
  deriving (Show, Eq)

-- ----------------------------------------------------------------
-- Constants
-- ----------------------------------------------------------------

-- | Maximum expansion iterations.
epaMaxIter :: Int
epaMaxIter = 64

-- | Convergence threshold: expansion stops when the support
-- distance and face distance agree within this tolerance.
epaTolerance :: Float
epaTolerance = 1.0e-4

-- ----------------------------------------------------------------
-- Face
-- ----------------------------------------------------------------

-- | A triangular face on the expanding polytope.
-- Stores vertices, outward unit normal, and signed distance
-- from the origin to the face plane.
data Face = Face !V3 !V3 !V3 !V3 !Float

-- ----------------------------------------------------------------
-- Entry point
-- ----------------------------------------------------------------

-- | Compute the penetration depth and direction using EPA.
--
-- The simplex must be a tetrahedron (4 points) enclosing the
-- origin, as returned by 'gjkSimplex'.  Returns 'Nothing' if
-- the simplex is degenerate or shapes do not overlap.
epa :: Support -> Support -> Simplex -> Maybe Penetration
epa _ _ (Simplex1 _) = Nothing
epa _ _ (Simplex2 _ _) = Nothing
epa _ _ Simplex3 {} = Nothing
epa (Support supA) (Support supB) (Simplex4 a b c d) =
  let initialFaces = orientFaces a b c d
   in epaLoop supA supB initialFaces Nothing 0

-- ----------------------------------------------------------------
-- Main loop
-- ----------------------------------------------------------------

epaLoop ::
  (V3 -> V3) ->
  (V3 -> V3) ->
  [Face] ->
  -- | Best penetration seen so far (minimum support distance).
  Maybe Penetration ->
  Int ->
  Maybe Penetration
epaLoop _ _ [] best _ = best
epaLoop _ _ _ best iter | iter >= epaMaxIter = best
epaLoop supA supB faces best iter =
  let -- Closest face to the origin
      (Face fa _ _ fn fd) = closestFace faces
      -- Support point along the face normal
      newPoint = supA fn ^-^ supB (negV fn)
      -- Support distance: the true penetration depth in this direction
      supportDist = dot fn newPoint
      -- Track the minimum support distance across all iterations.
      -- This is the key insight: even when face distances are near
      -- zero (degenerate polytope), the support distance is always
      -- geometrically valid.
      newBest = case best of
        Nothing -> Just (Penetration fn supportDist)
        Just prev
          | supportDist < penDepth prev ->
              Just (Penetration fn supportDist)
          | otherwise -> best
   in if supportDist - fd < epaTolerance
        then -- Converged: face distance matches support distance.
          Just (Penetration fn fd)
        else -- Expand the polytope toward the new support point.
          let (visible, hidden) =
                partition'
                  (\(Face _ _ _ n _) -> dot n (newPoint ^-^ fa) > 0)
                  faces
              horizonEdges = findHorizon visible
              newFaces = map (makeNewFace newPoint) horizonEdges
           in epaLoop supA supB (hidden ++ newFaces) newBest (iter + 1)

-- ----------------------------------------------------------------
-- Polytope construction
-- ----------------------------------------------------------------

-- | Build the 4 faces of a tetrahedron with outward normals.
orientFaces :: V3 -> V3 -> V3 -> V3 -> [Face]
orientFaces a b c d =
  let makeFace p0 p1 p2 opposite =
        let e1 = p1 ^-^ p0
            e2 = p2 ^-^ p0
            n = cross e1 e2
            toOpp = opposite ^-^ p0
            correctedN = if dot n toOpp > 0 then negV n else n
            nLen = vlength correctedN
            unitN =
              if nLen > 0
                then (1.0 / nLen) *^ correctedN
                else V3 0 1 0
            dist = dot unitN p0
         in if dot n toOpp > 0
              then Face p0 p2 p1 unitN (abs dist)
              else Face p0 p1 p2 unitN (abs dist)
   in [ makeFace a b c d,
        makeFace a c d b,
        makeFace a d b c,
        makeFace b d c a
      ]

-- | Find the face closest to the origin.
closestFace :: [Face] -> Face
closestFace = minimumBy (comparing (\(Face _ _ _ _ d) -> d))

-- | Make a new face from the new support point and a horizon edge.
makeNewFace :: V3 -> (V3, V3) -> Face
makeNewFace p (e0, e1) =
  let e01 = e1 ^-^ e0
      ep = p ^-^ e0
      n = cross e01 ep
      nLen = vlength n
      unitN =
        if nLen > 0
          then (1.0 / nLen) *^ n
          else V3 0 1 0
      dist = abs (dot unitN e0)
   in Face e0 e1 p unitN dist

-- ----------------------------------------------------------------
-- Horizon extraction
-- ----------------------------------------------------------------

-- | Find the horizon edges of visible faces.
-- An edge is on the horizon if it belongs to exactly one visible
-- face.
findHorizon :: [Face] -> [(V3, V3)]
findHorizon visible =
  let edges = concatMap faceEdges visible
      counts = foldl' countEdge [] edges
   in [e | (e, 1 :: Int) <- counts]
  where
    faceEdges (Face a b c _ _) = [(a, b), (b, c), (c, a)]

    edgeEq (a1, b1) (a2, b2) =
      (approxV3 a1 a2 && approxV3 b1 b2)
        || (approxV3 a1 b2 && approxV3 b1 a2)

    approxV3 (V3 x1 y1 z1) (V3 x2 y2 z2) =
      abs (x1 - x2) < 1.0e-8
        && abs (y1 - y2) < 1.0e-8
        && abs (z1 - z2) < 1.0e-8

    countEdge [] e = [(e, 1)]
    countEdge ((e', n) : rest) e
      | edgeEq e e' = (e', n + 1) : rest
      | otherwise = (e', n) : countEdge rest e

-- ----------------------------------------------------------------
-- Helpers
-- ----------------------------------------------------------------

negV :: V3 -> V3
negV (V3 x y z) = V3 (negate x) (negate y) (negate z)

partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' _ [] = ([], [])
partition' f (x : xs) =
  let (yes, no) = partition' f xs
   in if f x then (x : yes, no) else (yes, x : no)
