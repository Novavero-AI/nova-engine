-- | Mirror and radial symmetry operations.
--
-- Reflect meshes across coordinate planes or arbitrary planes,
-- and create radially symmetric copies around an axis.
module NovaEngine.Mesh.Symmetry
  ( -- * Axis-aligned mirrors
    mirrorX,
    mirrorY,
    mirrorZ,

    -- * Arbitrary plane mirror
    mirrorPlane,

    -- * Radial symmetry
    radialSymmetry,
  )
where

import NovaEngine.Mesh.Types

-- ----------------------------------------------------------------
-- Internal helpers
-- ----------------------------------------------------------------

-- | Apply a function to every vertex in a mesh, preserving
-- indices and vertex count.
mapVertices :: (Vertex -> Vertex) -> Mesh -> Mesh
mapVertices f (Mesh vs is c) = Mesh (map f vs) is c

-- | Reverse the winding order of every triangle by swapping the
-- second and third index of each triple.
reverseWindingRaw :: Mesh -> Mesh
reverseWindingRaw (Mesh vs is c) = Mesh vs (swapPairs is) c
  where
    swapPairs (a : b : idxC : rest) = a : idxC : b : swapPairs rest
    swapPairs remaining = remaining

-- ----------------------------------------------------------------
-- Axis-aligned mirrors
-- ----------------------------------------------------------------

-- | Mirror across the @X = 0@ plane and merge with the original.
--
-- The mirrored copy has negated X components for position, normal,
-- and tangent, with reversed winding to maintain correct face
-- orientation.
mirrorX :: Mesh -> Mesh
mirrorX mesh = mesh <> mirrorCopy
  where
    mirrorCopy = reverseWindingRaw $ mapVertices mirrorVert mesh
    mirrorVert v =
      v
        { vPosition = let V3 x y z = vPosition v in V3 (negate x) y z,
          vNormal = let V3 nx ny nz = vNormal v in V3 (negate nx) ny nz,
          vTangent = let V4 tx ty tz tw = vTangent v in V4 (negate tx) ty tz (negate tw)
        }

-- | Mirror across the @Y = 0@ plane and merge with the original.
--
-- The mirrored copy has negated Y components for position, normal,
-- and tangent, with reversed winding to maintain correct face
-- orientation.
mirrorY :: Mesh -> Mesh
mirrorY mesh = mesh <> mirrorCopy
  where
    mirrorCopy = reverseWindingRaw $ mapVertices mirrorVert mesh
    mirrorVert v =
      v
        { vPosition = let V3 x y z = vPosition v in V3 x (negate y) z,
          vNormal = let V3 nx ny nz = vNormal v in V3 nx (negate ny) nz,
          vTangent = let V4 tx ty tz tw = vTangent v in V4 tx (negate ty) tz (negate tw)
        }

-- | Mirror across the @Z = 0@ plane and merge with the original.
--
-- The mirrored copy has negated Z components for position, normal,
-- and tangent, with reversed winding to maintain correct face
-- orientation.
mirrorZ :: Mesh -> Mesh
mirrorZ mesh = mesh <> mirrorCopy
  where
    mirrorCopy = reverseWindingRaw $ mapVertices mirrorVert mesh
    mirrorVert v =
      v
        { vPosition = let V3 x y z = vPosition v in V3 x y (negate z),
          vNormal = let V3 nx ny nz = vNormal v in V3 nx ny (negate nz),
          vTangent = let V4 tx ty tz tw = vTangent v in V4 tx ty (negate tz) (negate tw)
        }

-- ----------------------------------------------------------------
-- Arbitrary plane mirror
-- ----------------------------------------------------------------

-- | Mirror across an arbitrary plane and merge with the original.
--
-- The plane is defined by a unit normal @n@ and signed distance
-- @d@ from the origin (points on the plane satisfy
-- @dot(n, p) = d@).
--
-- Positions are reflected: @p\' = p - 2*(dot(n,p) - d)*n@.
-- Normals and tangent xyz are reflected as direction vectors:
-- @v\' = v - 2*dot(n,v)*n@.  Winding is reversed on the mirrored
-- copy.
mirrorPlane :: V3 -> Float -> Mesh -> Mesh
mirrorPlane planeN d mesh = mesh <> mirrorCopy
  where
    mirrorCopy = reverseWindingRaw $ mapVertices mirrorVert mesh
    mirrorVert v =
      v
        { vPosition = reflectPoint planeN d (vPosition v),
          vNormal = reflectDir planeN (vNormal v),
          vTangent = reflectTangent planeN (vTangent v)
        }

-- | Reflect a position across a plane defined by normal @n@ and
-- distance @d@.
reflectPoint :: V3 -> Float -> V3 -> V3
reflectPoint n d p =
  p ^-^ (2.0 * (dot n p - d)) *^ n

-- | Reflect a direction vector across a plane normal.
reflectDir :: V3 -> V3 -> V3
reflectDir n v =
  v ^-^ (2.0 * dot n v) *^ n

-- | Reflect the xyz components of a tangent vector, negating
-- the w (bitangent handedness) since reflection flips chirality.
reflectTangent :: V3 -> V4 -> V4
reflectTangent n (V4 tx ty tz tw) =
  let V3 rx ry rz = reflectDir n (V3 tx ty tz)
   in V4 rx ry rz (negate tw)

-- ----------------------------------------------------------------
-- Radial symmetry
-- ----------------------------------------------------------------

-- | Create @n@ rotationally symmetric copies around an axis.
--
-- The original mesh is copy 0. Copies @1@ through @n-1@ are
-- rotated by @i * 2*pi/n@ radians around the given axis
-- (which should be a unit vector). All copies are merged via
-- the 'Mesh' 'Monoid'.
radialSymmetry :: V3 -> Int -> Mesh -> Mesh
radialSymmetry _axis n mesh
  | n <= 1 = mesh
radialSymmetry axis n mesh =
  mconcat (mesh : rotatedCopies)
  where
    step = 2.0 * pi / fromIntegral n
    rotatedCopies =
      [ mapVertices (rotateVertex q) mesh
      | i <- [1 .. n - 1],
        let angle = fromIntegral i * step,
        let q = axisAngle axis angle
      ]

-- | Rotate all spatial attributes of a vertex by a quaternion.
rotateVertex :: Quaternion -> Vertex -> Vertex
rotateVertex q v =
  v
    { vPosition = rotateV3 q (vPosition v),
      vNormal = normalize (rotateV3 q (vNormal v)),
      vTangent = rotateTangent q (vTangent v)
    }

-- | Rotate the xyz components of a tangent by a quaternion,
-- preserving the w (bitangent handedness).
rotateTangent :: Quaternion -> V4 -> V4
rotateTangent q (V4 tx ty tz tw) =
  let V3 rx ry rz = rotateV3 q (V3 tx ty tz)
   in V4 rx ry rz tw
