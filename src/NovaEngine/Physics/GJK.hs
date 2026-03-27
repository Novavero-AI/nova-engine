-- | GJK (Gilbert-Johnson-Keerthi) collision detection.
--
-- Determines whether two convex shapes overlap by constructing a
-- simplex in Minkowski difference space.  If the simplex encloses
-- the origin, the shapes intersect.
--
-- Pure, allocation-light, suitable for real-time use.
module NovaEngine.Physics.GJK
  ( -- * Support function
    Support (..),

    -- * Collision test
    gjkIntersect,

    -- * Simplex (exported for EPA)
    Simplex (..),
    gjkSimplex,
  )
where

import NovaEngine.Math.Types
import NovaEngine.Math.Vector (cross, dot, vlengthSq)

-- ----------------------------------------------------------------
-- Support function
-- ----------------------------------------------------------------

-- | A convex shape that can compute its farthest point along a
-- direction.  This is the only interface GJK needs — any convex
-- geometry (sphere, box, hull, capsule) just implements 'support'.
newtype Support = Support {support :: V3 -> V3}

-- ----------------------------------------------------------------
-- Simplex
-- ----------------------------------------------------------------

-- | A simplex of 1–4 vertices in Minkowski difference space.
data Simplex
  = Simplex1 !V3
  | Simplex2 !V3 !V3
  | Simplex3 !V3 !V3 !V3
  | Simplex4 !V3 !V3 !V3 !V3
  deriving (Show, Eq)

-- ----------------------------------------------------------------
-- GJK
-- ----------------------------------------------------------------

-- | Maximum iterations before giving up (prevents infinite loops
-- on degenerate inputs).
gjkMaxIter :: Int
gjkMaxIter = 32

-- | Test whether two convex shapes intersect.
gjkIntersect :: Support -> Support -> Bool
gjkIntersect a b = case gjkSimplex a b of
  Just _ -> True
  Nothing -> False

-- | Run GJK and return the final simplex if the shapes intersect.
-- The simplex is needed by EPA to compute penetration depth.
gjkSimplex :: Support -> Support -> Maybe Simplex
gjkSimplex (Support supA) (Support supB) =
  let -- Initial direction: (1,1,1) normalised. Off-axis to avoid
      -- degenerate GJK simplices when shapes are axis-aligned.
      inv3 = 1.0 / sqrt 3.0
      d0 = V3 inv3 inv3 inv3
      -- First Minkowski difference support point
      s0 = supA d0 ^-^ supB (negateDir d0)
   in if dot s0 d0 <= 0
        then Nothing
        else gjkLoop supA supB (Simplex1 s0) (negateDir s0) 0

gjkLoop ::
  (V3 -> V3) ->
  (V3 -> V3) ->
  Simplex ->
  V3 ->
  Int ->
  Maybe Simplex
gjkLoop _ _ _ _ iter | iter >= gjkMaxIter = Nothing
gjkLoop supA supB simplex dir iter =
  let a = supA dir ^-^ supB (negateDir dir)
   in if dot a dir <= 0
        then Nothing -- no intersection
        else case doSimplex (addPoint simplex a) of
          (newSimplex, _, True) -> Just newSimplex
          (newSimplex, newDir, False) ->
            gjkLoop supA supB newSimplex newDir (iter + 1)

-- | Process the simplex after adding a new point.
-- Returns (updated simplex, new search direction, contains origin?).
doSimplex :: Simplex -> (Simplex, V3, Bool)
doSimplex (Simplex1 a) =
  (Simplex1 a, negateDir a, False)
doSimplex (Simplex2 a b) =
  let ab = b ^-^ a
      ao = negateDir a
   in if dot ab ao > 0
        then
          let d = tripleProduct ab ao ab
           in if vlengthSq d < 1.0e-12
                then -- Origin is on line AB; pick any perpendicular.
                  (Simplex2 a b, perpTo ab, False)
                else (Simplex2 a b, d, False)
        else (Simplex1 a, ao, False)
doSimplex (Simplex3 a b c) =
  doTriangle a b c
doSimplex (Simplex4 a b c d) =
  doTetrahedron a b c d

-- | Triangle case: determine which Voronoi region the origin
-- is in and reduce the simplex accordingly.
doTriangle :: V3 -> V3 -> V3 -> (Simplex, V3, Bool)
doTriangle a b c =
  let ab = b ^-^ a
      ac = c ^-^ a
      ao = negateDir a
      abc = cross ab ac
      abcXac = cross abc ac
      abXabc = cross ab abc
   in if dot abcXac ao > 0
        then
          if dot ac ao > 0
            then (Simplex2 a c, cross (cross ac ao) ac, False)
            else doLine a b
        else
          if dot abXabc ao > 0
            then doLine a b
            else
              if dot abc ao > 0
                then (Simplex3 a b c, abc, False)
                else (Simplex3 a c b, negateDir abc, False)

-- | Line case: check if origin is in the direction of AB.
doLine :: V3 -> V3 -> (Simplex, V3, Bool)
doLine a b =
  let ab = b ^-^ a
      ao = negateDir a
   in if dot ab ao > 0
        then
          let d = tripleProduct ab ao ab
           in if vlengthSq d < 1.0e-12
                then (Simplex2 a b, perpTo ab, False)
                else (Simplex2 a b, d, False)
        else (Simplex1 a, ao, False)

-- | Tetrahedron case: check which face the origin is beyond.
doTetrahedron :: V3 -> V3 -> V3 -> V3 -> (Simplex, V3, Bool)
doTetrahedron a b c d =
  let ab = b ^-^ a
      ac = c ^-^ a
      ad = d ^-^ a
      ao = negateDir a
      abc = cross ab ac
      acd = cross ac ad
      adb = cross ad ab
   in if dot abc ao > 0
        then doTriangle a b c
        else
          if dot acd ao > 0
            then doTriangle a c d
            else
              if dot adb ao > 0
                then doTriangle a d b
                else -- Origin is inside the tetrahedron
                  (Simplex4 a b c d, V3 0 0 0, True)

-- ----------------------------------------------------------------
-- Helpers
-- ----------------------------------------------------------------

-- | Add a point to the front of a simplex, promoting it.
addPoint :: Simplex -> V3 -> Simplex
addPoint (Simplex1 b) a = Simplex2 a b
addPoint (Simplex2 b c) a = Simplex3 a b c
addPoint (Simplex3 b c d) a = Simplex4 a b c d
addPoint (Simplex4 _ c d e) a = Simplex4 a c d e

-- | Triple product: @cross (cross a b) c@.
tripleProduct :: V3 -> V3 -> V3 -> V3
tripleProduct a b = cross (cross a b)

-- | An arbitrary vector perpendicular to the given vector.
-- Uses off-axis reference vectors to avoid producing axis-aligned
-- results that would cause EPA face degeneracy.
perpTo :: V3 -> V3
perpTo v@(V3 x y z)
  -- Pick the reference vector least parallel to v, using
  -- off-axis directions to keep the result non-axis-aligned.
  | abs x <= abs y && abs x <= abs z = cross v (V3 1 0.1 0.1)
  | abs y <= abs z = cross v (V3 0.1 1 0.1)
  | otherwise = cross v (V3 0.1 0.1 1)

-- | Negate a direction vector.
negateDir :: V3 -> V3
negateDir (V3 x y z) = V3 (negate x) (negate y) (negate z)
