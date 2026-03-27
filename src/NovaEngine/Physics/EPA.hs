-- | EPA (Expanding Polytope Algorithm) for penetration depth.
--
-- Given a GJK simplex that encloses the origin (confirmed
-- intersection), EPA expands the polytope to find the minimum
-- translation vector (MTV) — the shortest direction and distance
-- to separate the two shapes.
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
    -- | Penetration depth.
    penDepth :: !Float
  }
  deriving (Show, Eq)

-- ----------------------------------------------------------------
-- EPA
-- ----------------------------------------------------------------

-- | Maximum iterations.
epaMaxIter :: Int
epaMaxIter = 32

-- | Tolerance for convergence.
epaTolerance :: Float
epaTolerance = 1.0e-4

-- | A triangular face on the expanding polytope.
data Face = Face !V3 !V3 !V3 !V3 !Float

-- vertices a b c, outward normal, distance to origin

-- | Compute the penetration depth and direction using EPA.
--
-- The simplex must be a tetrahedron (4 points) enclosing the
-- origin, as returned by 'gjkSimplex'.  Returns 'Nothing' if
-- the simplex is degenerate.
epa :: Support -> Support -> Simplex -> Maybe Penetration
epa _ _ (Simplex1 _) = Nothing
epa _ _ (Simplex2 _ _) = Nothing
epa _ _ Simplex3 {} = Nothing
epa (Support supA) (Support supB) (Simplex4 a b c d) =
  let -- Initial polytope: 4 faces of the tetrahedron.
      -- Winding must be outward-facing (normals point away from
      -- the interior).  We check and flip if needed.
      initialFaces = orientFaces a b c d
   in epaLoop supA supB initialFaces 0

-- | Build the 4 faces of a tetrahedron with outward normals.
orientFaces :: V3 -> V3 -> V3 -> V3 -> [Face]
orientFaces a b c d =
  let makeFace p0 p1 p2 opposite =
        let e1 = p1 ^-^ p0
            e2 = p2 ^-^ p0
            n = cross e1 e2
            -- Ensure normal points away from the opposite vertex
            toOpp = opposite ^-^ p0
            correctedN = if dot n toOpp > 0 then negV n else n
            nLen = vlength correctedN
            unitN = if nLen > 0 then (1.0 / nLen) *^ correctedN else V3 0 1 0
            dist = dot unitN p0
         in if dot n toOpp > 0
              then Face p0 p2 p1 unitN (abs dist)
              else Face p0 p1 p2 unitN (abs dist)
   in [ makeFace a b c d,
        makeFace a c d b,
        makeFace a d b c,
        makeFace b d c a
      ]

epaLoop ::
  (V3 -> V3) ->
  (V3 -> V3) ->
  [Face] ->
  Int ->
  Maybe Penetration
epaLoop _ _ [] _ = Nothing
epaLoop _ _ faces iter
  | iter >= epaMaxIter =
      let Face _ _ _ n d = closestFace faces
       in Just (Penetration n d)
epaLoop supA supB faces iter =
  let -- Find the face closest to the origin
      closest@(Face fa fb fc fn fd) = closestFace faces
      -- Get a new support point along the closest face's normal
      newPoint = supA fn ^-^ supB (negV fn)
      newDist = dot fn newPoint
   in if newDist - fd < epaTolerance
        then -- Converged
          Just (Penetration fn fd)
        else -- Expand: remove faces visible from the new point,
        -- add new faces connecting the new point to the horizon edges.
          let (visible, hidden) = partition' (\(Face _ _ _ n _) -> dot n (newPoint ^-^ fa) > 0) faces
              horizonEdges = findHorizon visible
              newFaces = map (makeNewFace newPoint) horizonEdges
           in epaLoop supA supB (hidden ++ newFaces) (iter + 1)

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
      unitN = if nLen > 0 then (1.0 / nLen) *^ n else V3 0 1 0
      dist = abs (dot unitN e0)
   in Face e0 e1 p unitN dist

-- | Find the horizon edges of visible faces.
-- An edge is on the horizon if it belongs to exactly one visible
-- face. We collect all edges from visible faces and keep those
-- that appear exactly once.
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

-- | Negate a V3.
negV :: V3 -> V3
negV (V3 x y z) = V3 (negate x) (negate y) (negate z)

-- | Partition a list (strict).
partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' _ [] = ([], [])
partition' f (x : xs) =
  let (yes, no) = partition' f xs
   in if f x then (x : yes, no) else (yes, x : no)
