-- | Mesh-level CSG boolean operations.
--
-- Union, intersection, and difference via centroid-based triangle
-- classification with ray casting (odd\/even rule) and
-- Moller-Trumbore intersection tests.
module NovaEngine.Mesh.Boolean
  ( -- * Boolean operations
    meshUnion,
    meshIntersection,
    meshDifference,
  )
where

import Data.Array (listArray, (!))
import NovaEngine.Mesh.Types

-- ----------------------------------------------------------------
-- Public API
-- ----------------------------------------------------------------

-- | Compute the union of two meshes (A + B).
--
-- Keeps triangles of A that are outside B and triangles of B
-- that are outside A.
meshUnion :: Mesh -> Mesh -> Mesh
meshUnion meshA meshB = vertexTrianglesToMesh (aOutside ++ bOutside)
  where
    posTriA = meshTriangles meshA
    posTriB = meshTriangles meshB
    vtriA = meshVertexTriangles meshA
    vtriB = meshVertexTriangles meshB
    (_, aOutside) = classifyTriangles posTriB vtriA
    (_, bOutside) = classifyTriangles posTriA vtriB

-- | Compute the intersection of two meshes (A AND B).
--
-- Keeps triangles of A that are inside B and triangles of B
-- that are inside A.
meshIntersection :: Mesh -> Mesh -> Mesh
meshIntersection meshA meshB = vertexTrianglesToMesh (aInside ++ bInside)
  where
    posTriA = meshTriangles meshA
    posTriB = meshTriangles meshB
    vtriA = meshVertexTriangles meshA
    vtriB = meshVertexTriangles meshB
    (aInside, _) = classifyTriangles posTriB vtriA
    (bInside, _) = classifyTriangles posTriA vtriB

-- | Compute the difference of two meshes (A minus B).
--
-- Keeps triangles of A that are outside B and flipped triangles
-- of B that are inside A (to form the carved interior surface).
meshDifference :: Mesh -> Mesh -> Mesh
meshDifference meshA meshB = vertexTrianglesToMesh (aOutside ++ map flipVertexTriangle bInside)
  where
    posTriA = meshTriangles meshA
    posTriB = meshTriangles meshB
    vtriA = meshVertexTriangles meshA
    vtriB = meshVertexTriangles meshB
    (_, aOutside) = classifyTriangles posTriB vtriA
    (bInside, _) = classifyTriangles posTriA vtriB

-- ----------------------------------------------------------------
-- Triangle extraction
-- ----------------------------------------------------------------

-- | Extract position triples from mesh triangles.
meshTriangles :: Mesh -> [(V3, V3, V3)]
meshTriangles (Mesh verts idxs _) =
  map toPosTri (groupTriangles idxs)
  where
    vertArr = listToFn verts
    toPosTri (a, b, c) =
      ( vPosition (vertArr (fromIntegral a)),
        vPosition (vertArr (fromIntegral b)),
        vPosition (vertArr (fromIntegral c))
      )

-- | Extract full vertex triples from mesh triangles.
meshVertexTriangles :: Mesh -> [(Vertex, Vertex, Vertex)]
meshVertexTriangles (Mesh verts idxs _) =
  map toVertTri (groupTriangles idxs)
  where
    vertArr = listToFn verts
    toVertTri (a, b, c) =
      ( vertArr (fromIntegral a),
        vertArr (fromIntegral b),
        vertArr (fromIntegral c)
      )

-- | Index into a list by position. Converts the list to an array
-- for O(1) repeated lookups. Returns a function that yields a
-- default vertex for any index if the list is empty.
listToFn :: [Vertex] -> Int -> Vertex
listToFn [] = const emptyVertex
listToFn xs = (arr !)
  where
    arr = listArray (0, length xs - 1) xs

-- | Sentinel vertex used when indexing into an empty mesh. All
-- fields are zero/default so downstream computations are safe.
emptyVertex :: Vertex
emptyVertex = vertex (V3 0 0 0) (V3 0 1 0) (V2 0 0) (V4 1 0 0 1)

-- ----------------------------------------------------------------
-- Ray casting
-- ----------------------------------------------------------------

-- | Moller-Trumbore ray-triangle intersection test.
--
-- Returns 'True' if a ray from the given origin in the given
-- direction intersects the triangle (in front of the origin).
rayHitsTriangle :: V3 -> V3 -> (V3, V3, V3) -> Bool
rayHitsTriangle origin direction (v0, v1, v2) =
  let edge1 = v1 ^-^ v0
      edge2 = v2 ^-^ v0
      h = cross direction edge2
      a = dot edge1 h
   in abs a >= nearZeroLength
        && let f = 1.0 / a
               s = origin ^-^ v0
               u = f * dot s h
            in u >= 0.0
                 && u <= 1.0
                 && let q = cross s edge1
                        v = f * dot direction q
                     in v >= 0.0
                          && u + v <= 1.0
                          && let t = f * dot edge2 q
                              in t > nearZeroLength

-- | Test whether a point is inside a closed mesh using the
-- ray-casting (odd\/even) rule.
--
-- Casts a ray in a slightly off-axis direction to reduce edge-case
-- coincidences, and counts intersections.
isInsideMesh :: [(V3, V3, V3)] -> V3 -> Bool
isInsideMesh tris point =
  odd (length (filter (rayHitsTriangle point rayDir) tris))
  where
    rayDir = normalize (V3 1.0 0.00013 0.00027)

-- ----------------------------------------------------------------
-- Classification
-- ----------------------------------------------------------------

-- | Compute the centroid of a triangle.
triCentroid :: V3 -> V3 -> V3 -> V3
triCentroid a b c = (1.0 / 3.0) *^ (a ^+^ b ^+^ c)

-- | Classify vertex-triangles as inside or outside a reference mesh.
--
-- For each triangle, the centroid of its vertex positions is tested
-- against the reference mesh's triangles. Returns (inside, outside).
classifyTriangles ::
  -- | Reference mesh triangles (position triples)
  [(V3, V3, V3)] ->
  -- | Triangles to classify (vertex triples)
  [(Vertex, Vertex, Vertex)] ->
  ([(Vertex, Vertex, Vertex)], [(Vertex, Vertex, Vertex)])
classifyTriangles refTris = foldr classify ([], [])
  where
    classify tri@(va, vb, vc) (ins, outs) =
      let centroid = triCentroid (vPosition va) (vPosition vb) (vPosition vc)
       in if isInsideMesh refTris centroid
            then (tri : ins, outs)
            else (ins, tri : outs)

-- ----------------------------------------------------------------
-- Triangle manipulation
-- ----------------------------------------------------------------

-- | Flip a vertex triangle's winding order by swapping the second
-- and third vertices, and negate all vertex normals.
flipVertexTriangle :: (Vertex, Vertex, Vertex) -> (Vertex, Vertex, Vertex)
flipVertexTriangle (va, vb, vc) =
  (negNormal va, negNormal vc, negNormal vb)
  where
    negNormal v =
      let V3 nx ny nz = vNormal v
       in v {vNormal = V3 (negate nx) (negate ny) (negate nz)}

-- | Build a mesh from vertex triples. Each triple becomes one
-- triangle with sequential indices.
vertexTrianglesToMesh :: [(Vertex, Vertex, Vertex)] -> Mesh
vertexTrianglesToMesh tris = mkMesh verts idxs
  where
    verts = concatMap (\(a, b, c) -> [a, b, c]) tris
    idxs = [0 .. fromIntegral (length verts - 1)] :: [Word32]
