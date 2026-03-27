{-# OPTIONS_GHC -fno-warn-orphans #-}

{- HLINT ignore "Monoid law, left identity" -}
{- HLINT ignore "Monoid law, right identity" -}

module Main (main) where

import Data.Bits (shiftL)
import Data.IntMap.Strict qualified as IntMap
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Word (Word64)
import Foreign.Storable (sizeOf)
import NovaEngine.Animation.Animate
import NovaEngine.Animation.IK hiding (lookAt)
import NovaEngine.Animation.Morph
import NovaEngine.Animation.Pose
import NovaEngine.Animation.Skeleton
import NovaEngine.Animation.Skin
import NovaEngine.Math.Matrix
  ( identity,
    inverse,
    lookAt,
    mulM44,
    ortho,
    perspective,
    rotation,
    scaling,
    translation,
  )
import NovaEngine.Math.Types (M44 (..))
import NovaEngine.Mesh.Boolean
import NovaEngine.Mesh.Combine
import NovaEngine.Mesh.Curve
import NovaEngine.Mesh.Deform
import NovaEngine.Mesh.Export
import NovaEngine.Mesh.Hull
import NovaEngine.Mesh.Icosphere
import NovaEngine.Mesh.Import
import NovaEngine.Mesh.LOD
import NovaEngine.Mesh.Loft
import NovaEngine.Mesh.Primitives
import NovaEngine.Mesh.Remesh
import NovaEngine.Mesh.Simplify
import NovaEngine.Mesh.Smooth
import NovaEngine.Mesh.Subdivision
import NovaEngine.Mesh.Surface
import NovaEngine.Mesh.Symmetry
import NovaEngine.Mesh.Types
import NovaEngine.Mesh.UV
import NovaEngine.Mesh.Weld
import NovaEngine.Noise
import NovaEngine.Render.Material
import NovaEngine.Render.Skin (SkinnedVertex (..))
import NovaEngine.Render.Texture (calcMipLevels)
import NovaEngine.SDF
import NovaEngine.SDF.DualContour
import NovaEngine.SDF.Isosurface
import NovaEngine.Scene
import NovaEngine.Scene.Camera
import NovaEngine.Scene.FrameUBO (FrameUBO (..))
import NovaEngine.Scene.Shadow
import NovaEngine.Spatial.Raycast
import NovaEngine.Terrain
import NovaEngine.Terrain.Scatter
import Test.Tasty
import Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "nova-engine"
    [ testGroup "Types" typesTests,
      testGroup "Combine" combineTests,
      testGroup "Primitives" primitivesTests,
      testGroup "Curve" curveTests,
      testGroup "Surface" surfaceTests,
      testGroup "Loft" loftTests,
      testGroup "SDF" sdfTests,
      testGroup "Isosurface" isosurfaceTests,
      testGroup "Subdivision" subdivisionTests,
      testGroup "Deform" deformTests,
      testGroup "Noise" noiseTests,
      testGroup "Skeleton" skeletonTests,
      testGroup "Pose" poseTests,
      testGroup "Animate" animateTests,
      testGroup "IK" ikTests,
      testGroup "Skin" skinTests,
      testGroup "Morph" morphTests,
      testGroup "DualContour" dualContourTests,
      testGroup "Hull" hullTests,
      testGroup "Simplify" simplifyTests,
      testGroup "Smooth" smoothTests,
      testGroup "Weld" weldTests,
      testGroup "Icosphere" icosphereTests,
      testGroup "Export" exportTests,
      testGroup "Terrain" terrainTests,
      testGroup "UV" uvTests,
      testGroup "Boolean" booleanTests,
      testGroup "Scatter" scatterTests,
      testGroup "Symmetry" symmetryTests,
      testGroup "LOD" lodTests,
      testGroup "Raycast" raycastTests,
      testGroup "Remesh" remeshTests,
      testGroup "Import" importTests,
      testGroup "Texture" textureTests,
      testGroup "Scene" sceneTests,
      testGroup "Camera" cameraTests,
      testGroup "Material" materialTests,
      testGroup "ShadowCascade" shadowCascadeTests,
      testGroup "StorableLayout" storableLayoutTests
    ]

-- ----------------------------------------------------------------
-- Arbitrary instances
-- ----------------------------------------------------------------

smallFloat :: Gen Float
smallFloat = choose (-100, 100)

instance Arbitrary V2 where
  arbitrary = V2 <$> smallFloat <*> smallFloat

instance Arbitrary V3 where
  arbitrary = V3 <$> smallFloat <*> smallFloat <*> smallFloat

instance Arbitrary V4 where
  arbitrary = V4 <$> smallFloat <*> smallFloat <*> smallFloat <*> smallFloat

arbitraryUnitV3 :: Gen V3
arbitraryUnitV3 = do
  x <- smallFloat
  y <- smallFloat
  z <- smallFloat
  let v = V3 x y z
      len = vlength v
  if len < 0.001
    then pure (V3 0 1 0)
    else pure (normalize v)

instance Arbitrary Quaternion where
  arbitrary = axisAngle <$> arbitraryUnitV3 <*> smallFloat

instance Arbitrary Vertex where
  arbitrary = do
    pos <- arbitrary
    normal <- arbitraryUnitV3
    uvU <- choose (0, 1)
    uvV <- choose (0, 1)
    tangentDir <- arbitraryUnitV3
    w <- elements [1.0, -1.0]
    let V3 tx ty tz = tangentDir
    pure (vertex pos normal (V2 uvU uvV) (V4 tx ty tz w))

instance Arbitrary Mesh where
  arbitrary = do
    vertexCount <- choose (3, 30)
    vertices <- vectorOf vertexCount arbitrary
    triangleCount <- choose (1, 10)
    indices <-
      vectorOf
        (triangleCount * 3)
        (choose (0, fromIntegral vertexCount - 1))
    pure (mkMesh vertices indices)

-- ----------------------------------------------------------------
-- Helpers
-- ----------------------------------------------------------------

approxEq :: Float -> Float -> Bool
approxEq a b = abs (a - b) < tolerance
  where
    tolerance = 1.0e-4

approxEqV3 :: V3 -> V3 -> Bool
approxEqV3 (V3 x1 y1 z1) (V3 x2 y2 z2) =
  approxEq x1 x2 && approxEq y1 y2 && approxEq z1 z2

-- | Approximate quaternion equality (accounts for double-cover:
-- q and -q represent the same rotation).
approxEqQuat :: Quaternion -> Quaternion -> Bool
approxEqQuat (Quaternion w1 (V3 x1 y1 z1)) (Quaternion w2 (V3 x2 y2 z2)) =
  let d = abs (w1 * w2 + x1 * x2 + y1 * y2 + z1 * z2)
   in d > 0.999

-- | Approximate M44 equality.
approxEqM44 :: M44 -> M44 -> Bool
approxEqM44 (M44 (V4 a00 a10 a20 a30) (V4 a01 a11 a21 a31) (V4 a02 a12 a22 a32) (V4 a03 a13 a23 a33)) (M44 (V4 b00 b10 b20 b30) (V4 b01 b11 b21 b31) (V4 b02 b12 b22 b32) (V4 b03 b13 b23 b33)) =
  all
    (uncurry approxEq)
    [ (a00, b00),
      (a10, b10),
      (a20, b20),
      (a30, b30),
      (a01, b01),
      (a11, b11),
      (a21, b21),
      (a31, b31),
      (a02, b02),
      (a12, b12),
      (a22, b22),
      (a32, b32),
      (a03, b03),
      (a13, b13),
      (a23, b23),
      (a33, b33)
    ]

-- | Approximate Transform equality.
approxEqTransform :: Transform -> Transform -> Bool
approxEqTransform a b =
  approxEqV3 (transformPosition a) (transformPosition b)
    && approxEqQuat (transformRotation a) (transformRotation b)
    && approxEqV3 (transformScale a) (transformScale b)

-- | Approximate pose equality.
approxEqPose :: Pose -> Pose -> Bool
approxEqPose pA pB =
  let allKeys = IntMap.union (unPose pA) (unPose pB)
   in all
        ( \jid ->
            let qA = IntMap.findWithDefault (Quaternion 1 vzero) jid (unPose pA)
                qB = IntMap.findWithDefault (Quaternion 1 vzero) jid (unPose pB)
             in approxEqQuat qA qB
        )
        (IntMap.keys allKeys)

-- | Check structural invariants on a generated mesh.
checkMesh :: Mesh -> Bool
checkMesh m =
  validIndices m
    && validTriangleCount m
    && validNormals 1.0e-4 m
    && meshVertexCount m == length (meshVertices m)
    && meshVertexCount m > 0
    && not (null (meshIndices m))

-- | Positive float generator for radii and dimensions.
positiveFloat :: Gen Float
positiveFloat = choose (0.1, 10.0)

-- | Tessellation parameter generator.
tessParam :: Gen Int
tessParam = choose (3, 20)

-- | Tessellation parameter for stacks/segments (can be lower).
stackParam :: Gen Int
stackParam = choose (1, 20)

-- ----------------------------------------------------------------
-- Types tests
-- ----------------------------------------------------------------

typesTests :: [TestTree]
typesTests =
  [ QC.testProperty "V3 vzero is additive identity" $
      \v -> approxEqV3 ((v :: V3) ^+^ vzero) v,
    QC.testProperty "V3 addition is commutative" $
      \a b -> approxEqV3 ((a :: V3) ^+^ b) (b ^+^ a),
    QC.testProperty "V3 scalar multiply by 1 is identity" $
      \v -> approxEqV3 (1.0 *^ (v :: V3)) v,
    QC.testProperty "V3 scalar multiply by 0 gives zero" $
      \v -> approxEqV3 (0.0 *^ (v :: V3)) vzero,
    QC.testProperty "dot product is commutative" $
      \a b -> approxEq (dot (a :: V3) b) (dot b a),
    QC.testProperty "cross product is antisymmetric" $
      \a b ->
        approxEqV3 (cross (a :: V3) b) ((-1) *^ cross b a),
    QC.testProperty "cross product is orthogonal to both inputs" $
      forAll ((,) <$> arbitraryUnitV3 <*> arbitraryUnitV3) $ \(a, b) ->
        let c = cross a b
         in approxEq (dot a c) 0 && approxEq (dot b c) 0,
    QC.testProperty "normalize produces unit length" $
      \v ->
        vlength (v :: V3) > 0.01 QC.==>
          approxEq (vlength (normalize v)) 1.0,
    QC.testProperty "normalize is idempotent" $
      \v ->
        vlength (v :: V3) > 0.01 QC.==>
          approxEqV3 (normalize (normalize v)) (normalize v),
    QC.testProperty "quaternion rotation preserves length" $
      \q v ->
        approxEq
          (vlength (v :: V3))
          (vlength (rotateV3 (q :: Quaternion) v)),
    QC.testProperty "identity quaternion is identity" $
      \v ->
        let identityQ = axisAngle (V3 0 1 0) 0
         in approxEqV3 (rotateV3 identityQ (v :: V3)) v,
    QC.testProperty "Mesh mempty is left identity" $
      \m ->
        meshVertices (mempty <> (m :: Mesh)) == meshVertices m
          && meshIndices (mempty <> m) == meshIndices m,
    QC.testProperty "Mesh mempty is right identity" $
      \m ->
        meshVertices ((m :: Mesh) <> mempty) == meshVertices m
          && meshIndices (m <> mempty) == meshIndices m,
    QC.testProperty "Mesh merge preserves vertex count" $
      \a b ->
        length (meshVertices ((a :: Mesh) <> b))
          == length (meshVertices a) + length (meshVertices b),
    QC.testProperty "Mesh merge preserves index count" $
      \a b ->
        length (meshIndices ((a :: Mesh) <> b))
          == length (meshIndices a) + length (meshIndices b),
    QC.testProperty "Mesh merge preserves index validity" $
      \a b ->
        validIndices (a :: Mesh) && validIndices b QC.==>
          validIndices (a <> b),
    QC.testProperty "generated meshes have valid indices" $
      \m -> validIndices (m :: Mesh),
    QC.testProperty "generated meshes have valid triangle count" $
      \m -> validTriangleCount (m :: Mesh),
    QC.testProperty "generated meshes have valid normals" $
      \m -> validNormals 1.0e-5 (m :: Mesh)
  ]

-- ----------------------------------------------------------------
-- Combine tests
-- ----------------------------------------------------------------

combineTests :: [TestTree]
combineTests =
  [ QC.testProperty "translate by zero is identity" $
      \m -> translate vzero (m :: Mesh) == m,
    QC.testProperty "translate preserves vertex count" $
      \v m ->
        length (meshVertices (translate (v :: V3) (m :: Mesh)))
          == length (meshVertices m),
    QC.testProperty "translate preserves index validity" $
      \v m ->
        validIndices (m :: Mesh) QC.==>
          validIndices (translate (v :: V3) m),
    QC.testProperty "uniformScale by 1 is identity" $
      \m -> uniformScale 1.0 (m :: Mesh) == m,
    QC.testProperty "uniformScale preserves index validity" $
      \s m ->
        validIndices (m :: Mesh) QC.==>
          validIndices (uniformScale (s :: Float) m),
    QC.testProperty "flipNormals is involutory" $
      \m -> flipNormals (flipNormals (m :: Mesh)) == m,
    QC.testProperty "reverseWinding is involutory" $
      \m -> reverseWinding (reverseWinding (m :: Mesh)) == m,
    QC.testProperty "merge empty list gives mempty" $
      merge [] == (mempty :: Mesh),
    QC.testProperty "merge singleton is identity" $
      \m -> merge [m :: Mesh] == m,
    QC.testProperty "rotate preserves vertex count" $
      \q m ->
        length (meshVertices (rotate (q :: Quaternion) (m :: Mesh)))
          == length (meshVertices m),
    QC.testProperty "rotate preserves index validity" $
      \q m ->
        validIndices (m :: Mesh) QC.==>
          validIndices (rotate (q :: Quaternion) m)
  ]

-- ----------------------------------------------------------------
-- Primitives tests
-- ----------------------------------------------------------------

primitivesTests :: [TestTree]
primitivesTests =
  [ -- Sphere
    QC.testProperty "sphere rejects non-positive radius" $
      forAll (choose (-10.0, 0.0)) $ \r ->
        isNothing (sphere r 8 4),
    QC.testProperty "sphere produces valid mesh" $
      forAll ((,,) <$> positiveFloat <*> tessParam <*> stackParam) $
        \(r, sl, st) ->
          maybe False checkMesh (sphere r sl st),
    QC.testProperty "sphere vertex count matches formula" $
      forAll ((,,) <$> positiveFloat <*> tessParam <*> stackParam) $
        \(r, slRaw, stRaw) ->
          let sl = max 3 slRaw
              st = max 2 stRaw
              expected = 2 * sl + (st - 1) * (sl + 1)
           in case sphere r slRaw stRaw of
                Just m -> meshVertexCount m == expected
                Nothing -> False,
    QC.testProperty "sphere index count matches formula" $
      forAll ((,,) <$> positiveFloat <*> tessParam <*> stackParam) $
        \(r, slRaw, stRaw) ->
          let sl = max 3 slRaw
              st = max 2 stRaw
              expected = 6 * (st - 1) * sl
           in case sphere r slRaw stRaw of
                Just m -> length (meshIndices m) == expected
                Nothing -> False,
    QC.testProperty "sphere vertices within bounding sphere" $
      forAll ((,,) <$> positiveFloat <*> tessParam <*> stackParam) $
        \(r, sl, st) ->
          case sphere r sl st of
            Just m ->
              all
                (\v -> vlength (vPosition v) <= r + 1.0e-4)
                (meshVertices m)
            Nothing -> False,
    -- Capsule
    QC.testProperty "capsule rejects non-positive params" $
      forAll (choose (-10.0, 0.0)) $ \r ->
        isNothing (capsule r 1.0 8 4 2)
          && isNothing (capsule 1.0 r 8 4 2),
    QC.testProperty "capsule produces valid mesh" $
      forAll ((,) <$> positiveFloat <*> positiveFloat) $ \(r, h) ->
        maybe False checkMesh (capsule r h 8 4 2),
    QC.testProperty "capsule vertex count matches formula"
      $ forAll
        ( (,,,,)
            <$> positiveFloat
            <*> positiveFloat
            <*> tessParam
            <*> stackParam
            <*> stackParam
        )
      $ \(r, h, slRaw, hrRaw, brRaw) ->
        let sl = max 3 slRaw
            hr = max 1 hrRaw
            br = max 1 brRaw
            expected = 2 * sl + (2 * (hr - 1) + br + 1) * (sl + 1)
         in case capsule r h slRaw hrRaw brRaw of
              Just m -> meshVertexCount m == expected
              Nothing -> False,
    -- Cylinder
    QC.testProperty "cylinder rejects non-positive params" $
      forAll (choose (-10.0, 0.0)) $ \r ->
        isNothing (cylinder r 1.0 8 1 True True)
          && isNothing (cylinder 1.0 r 8 1 True True),
    QC.testProperty "cylinder produces valid mesh" $
      forAll ((,) <$> positiveFloat <*> positiveFloat) $ \(r, h) ->
        maybe False checkMesh (cylinder r h 8 1 True True),
    QC.testProperty "cylinder vertex count matches formula"
      $ forAll
        ( (,,,,)
            <$> positiveFloat
            <*> positiveFloat
            <*> tessParam
            <*> stackParam
            <*> elements [(True, True), (True, False), (False, True), (False, False)]
        )
      $ \(r, h, slRaw, hsRaw, (tc, bc)) ->
        let sl = max 3 slRaw
            hs = max 1 hsRaw
            capCount = (if tc then 1 else 0) + (if bc then 1 else 0)
            expected = (hs + 1) * (sl + 1) + capCount * (sl + 1)
         in case cylinder r h slRaw hsRaw tc bc of
              Just m -> meshVertexCount m == expected
              Nothing -> False,
    QC.testProperty "cylinder index count matches formula"
      $ forAll
        ( (,,,,)
            <$> positiveFloat
            <*> positiveFloat
            <*> tessParam
            <*> stackParam
            <*> elements [(True, True), (True, False), (False, True), (False, False)]
        )
      $ \(r, h, slRaw, hsRaw, (tc, bc)) ->
        let sl = max 3 slRaw
            hs = max 1 hsRaw
            capCount = (if tc then 1 else 0) + (if bc then 1 else 0)
            expected = 6 * hs * sl + capCount * 3 * sl
         in case cylinder r h slRaw hsRaw tc bc of
              Just m -> length (meshIndices m) == expected
              Nothing -> False,
    -- Cone
    QC.testProperty "cone rejects non-positive params" $
      forAll (choose (-10.0, 0.0)) $ \r ->
        isNothing (cone r 1.0 8 4 True)
          && isNothing (cone 1.0 r 8 4 True),
    QC.testProperty "cone produces valid mesh" $
      forAll ((,) <$> positiveFloat <*> positiveFloat) $ \(r, h) ->
        maybe False checkMesh (cone r h 8 4 True),
    QC.testProperty "cone vertex count matches formula"
      $ forAll
        ( (,,,,)
            <$> positiveFloat
            <*> positiveFloat
            <*> tessParam
            <*> stackParam
            <*> elements [True, False]
        )
      $ \(r, h, slRaw, stRaw, cap) ->
        let sl = max 3 slRaw
            st = max 1 stRaw
            capVerts = if cap then sl + 1 else 0
            expected = sl + st * (sl + 1) + capVerts
         in case cone r h slRaw stRaw cap of
              Just m -> meshVertexCount m == expected
              Nothing -> False,
    -- Torus
    QC.testProperty "torus rejects non-positive or invalid radii" $
      isNothing (torus 0 1 8 8)
        && isNothing (torus 1 0 8 8)
        && isNothing (torus 1 2 8 8),
    QC.testProperty "torus produces valid mesh"
      $ forAll
        ( do
            majR <- positiveFloat
            minR <- choose (0.1, majR)
            pure (majR, minR)
        )
      $ \(majR, minR) ->
        maybe False checkMesh (torus majR minR 8 8),
    QC.testProperty "torus vertex count matches formula"
      $ forAll
        ( do
            majR <- positiveFloat
            minR <- choose (0.1, majR)
            ri <- tessParam
            sl <- tessParam
            pure (majR, minR, ri, sl)
        )
      $ \(majR, minR, riRaw, slRaw) ->
        let ri = max 3 riRaw
            sl = max 3 slRaw
            expected = (ri + 1) * (sl + 1)
         in case torus majR minR riRaw slRaw of
              Just m -> meshVertexCount m == expected
              Nothing -> False,
    -- Box
    QC.testProperty "box rejects non-positive dimensions" $
      isNothing (box 0 1 1 1 1 1)
        && isNothing (box 1 0 1 1 1 1)
        && isNothing (box 1 1 0 1 1 1),
    QC.testProperty "box produces valid mesh" $
      forAll ((,,) <$> positiveFloat <*> positiveFloat <*> positiveFloat) $
        \(w, h, d) ->
          maybe False checkMesh (box w h d 1 1 1),
    QC.testProperty "box unit has 24 vertices and 36 indices" $
      case box 1 1 1 1 1 1 of
        Just m -> meshVertexCount m == 24 && length (meshIndices m) == 36
        Nothing -> False,
    -- Plane
    QC.testProperty "plane rejects non-positive dimensions" $
      isNothing (plane 0 1 1 1) && isNothing (plane 1 0 1 1),
    QC.testProperty "plane produces valid mesh" $
      forAll ((,) <$> positiveFloat <*> positiveFloat) $ \(w, d) ->
        maybe False checkMesh (plane w d 4 4),
    QC.testProperty "plane vertex count matches formula"
      $ forAll
        ( (,,,)
            <$> positiveFloat
            <*> positiveFloat
            <*> stackParam
            <*> stackParam
        )
      $ \(w, d, sxRaw, szRaw) ->
        let sx = max 1 sxRaw
            sz = max 1 szRaw
            expected = (sx + 1) * (sz + 1)
         in case plane w d sxRaw szRaw of
              Just m -> meshVertexCount m == expected
              Nothing -> False,
    -- Tapered Cylinder
    QC.testProperty "taperedCylinder rejects invalid params" $
      isNothing (taperedCylinder (-1) 1 1 8 1 True True)
        && isNothing (taperedCylinder 0 0 1 8 1 True True)
        && isNothing (taperedCylinder 1 1 0 8 1 True True),
    QC.testProperty "taperedCylinder produces valid mesh" $
      forAll ((,,) <$> positiveFloat <*> positiveFloat <*> positiveFloat) $
        \(topR, botR, h) ->
          maybe False checkMesh (taperedCylinder topR botR h 8 1 True True),
    QC.testProperty "taperedCylinder degenerates to cylinder" $
      forAll ((,) <$> positiveFloat <*> positiveFloat) $ \(r, h) ->
        case (taperedCylinder r r h 8 4 True True, cylinder r h 8 4 True True) of
          (Just tc, Just cyl) ->
            meshVertexCount tc == meshVertexCount cyl
              && length (meshIndices tc) == length (meshIndices cyl)
          _ -> False,
    -- Cross-cutting: all primitives produce isJust for valid params
    QC.testProperty "all primitives succeed with valid params" $
      forAll positiveFloat $ \r ->
        isJust (sphere r 8 4)
          && isJust (capsule r r 8 2 2)
          && isJust (cylinder r r 8 1 True True)
          && isJust (cone r r 8 4 True)
          && isJust (torus (r + 1) r 8 8)
          && isJust (box r r r 1 1 1)
          && isJust (plane r r 1 1)
          && isJust (taperedCylinder r (r * 0.5) r 8 1 True True)
  ]

-- ----------------------------------------------------------------
-- Curve tests
-- ----------------------------------------------------------------

-- | A simple cubic Bezier curve for testing.
testCubicBezier :: BezierCurve V3
testCubicBezier =
  BezierCurve
    [ V3 0 0 0,
      V3 1 2 0,
      V3 3 2 0,
      V3 4 0 0
    ]

-- | A quadratic Bezier curve for testing.
testQuadBezier :: BezierCurve V2
testQuadBezier =
  BezierCurve
    [V2 0 0, V2 1 2, V2 2 0]

-- | A clamped cubic B-spline for testing.
testBSpline :: BSplineCurve V3
testBSpline =
  BSplineCurve
    3
    (knotArray [0, 0, 0, 0, 1, 2, 2, 2, 2])
    [V3 0 0 0, V3 1 2 0, V3 2 2 0, V3 3 1 0, V3 4 0 0]

curveTests :: [TestTree]
curveTests =
  [ -- Bezier evaluation
    QC.testProperty "Bezier empty returns Nothing" $
      isNothing (evalBezier (BezierCurve [] :: BezierCurve V3) 0.5),
    QC.testProperty "Bezier single point returns that point" $
      evalBezier (BezierCurve [V3 1 2 3]) 0.5 == Just (V3 1 2 3),
    QC.testProperty "Bezier interpolates endpoints" $
      let pts = bezierControlPoints testCubicBezier
          start = evalBezier testCubicBezier 0.0
          end = evalBezier testCubicBezier 1.0
       in case (start, end, pts) of
            (Just s, Just e, first : _) ->
              approxEqV3 s first
                && approxEqV3 e (V3 4 0 0)
            _ -> False,
    QC.testProperty "Bezier midpoint is on curve" $
      isJust (evalBezier testCubicBezier 0.5),
    -- Bezier splitting
    QC.testProperty "Bezier split produces valid subcurves" $
      forAll (choose (0.0, 1.0)) $ \t ->
        case splitBezier testCubicBezier t of
          Just (left, right) ->
            length (bezierControlPoints left) == 4
              && length (bezierControlPoints right) == 4
          Nothing -> False,
    QC.testProperty "Bezier split left endpoint matches original start" $
      case splitBezier testCubicBezier 0.5 of
        Just (left, _) ->
          case evalBezier left 0.0 of
            Just pt -> approxEqV3 pt (V3 0 0 0)
            Nothing -> False
        Nothing -> False,
    -- Bezier derivative
    QC.testProperty "Bezier derivative of cubic is quadratic" $
      case bezierDerivative testCubicBezier of
        Just deriv -> length (bezierControlPoints deriv) == 3
        Nothing -> False,
    QC.testProperty "Bezier derivative of constant is zero" $
      let constCurve = BezierCurve [V3 1 1 1, V3 1 1 1, V3 1 1 1]
       in case evalBezierDerivative constCurve 0.5 of
            Just d -> approxEqV3 d vzero
            Nothing -> False,
    -- V2 Bezier
    QC.testProperty "V2 Bezier interpolates endpoints" $
      let start = evalBezier testQuadBezier 0.0
          end = evalBezier testQuadBezier 1.0
       in case (start, end) of
            (Just s, Just e) ->
              approxEq (let V2 x _ = s in x) 0
                && approxEq (let V2 x _ = e in x) 2
            _ -> False,
    -- B-spline
    QC.testProperty "B-spline evaluates at start" $
      case evalBSpline testBSpline 0.0 of
        Just pt -> approxEqV3 pt (V3 0 0 0)
        Nothing -> False,
    QC.testProperty "B-spline evaluates at end" $
      case evalBSpline testBSpline 2.0 of
        Just pt -> approxEqV3 pt (V3 4 0 0)
        Nothing -> False,
    QC.testProperty "B-spline rejects out-of-range" $
      isNothing (evalBSpline testBSpline (-0.1))
        && isNothing (evalBSpline testBSpline 2.1),
    QC.testProperty "B-spline derivative reduces degree" $
      case bsplineDerivative testBSpline of
        Just deriv -> bsplineDegree deriv == 2
        Nothing -> False,
    -- Arc-length
    QC.testProperty "arc-length table total is positive for non-degenerate curve" $
      let derivFn t = fromMaybe vzero (evalBezierDerivative testCubicBezier t)
          table = buildArcLengthTable derivFn vlength 100 0 1
       in totalArcLength table > 0,
    QC.testProperty "arc-length param at 0 maps to start" $
      let derivFn t = fromMaybe vzero (evalBezierDerivative testCubicBezier t)
          table = buildArcLengthTable derivFn vlength 100 0 1
       in approxEq (arcLengthToParam table 0.0) 0.0,
    QC.testProperty "arc-length param at total maps to end" $
      let derivFn t = fromMaybe vzero (evalBezierDerivative testCubicBezier t)
          table = buildArcLengthTable derivFn vlength 100 0 1
          arcTotal = totalArcLength table
       in approxEq (arcLengthToParam table arcTotal) 1.0,
    QC.testProperty "arc-length param is monotonically increasing" $
      let derivFn t = fromMaybe vzero (evalBezierDerivative testCubicBezier t)
          table = buildArcLengthTable derivFn vlength 100 0 1
          arcTotal = totalArcLength table
          samples = [arcLengthToParam table (arcTotal * fromIntegral i / 10.0) | i <- [0 .. 10 :: Int]]
       in and (zipWith (<=) samples (drop 1 samples))
  ]

-- ----------------------------------------------------------------
-- Surface tests
-- ----------------------------------------------------------------

-- | A flat bilinear Bezier patch (2x2 control points).
testBilinearPatch :: BezierPatch V3
testBilinearPatch =
  BezierPatch
    2
    2
    [ V3 0 0 0,
      V3 1 0 0,
      V3 0 0 1,
      V3 1 0 1
    ]

-- | A bicubic Bezier patch (4x4 control points) — a gentle hill.
testBicubicPatch :: BezierPatch V3
testBicubicPatch =
  BezierPatch
    4
    4
    [ V3 0 0 0,
      V3 1 0 0,
      V3 2 0 0,
      V3 3 0 0,
      V3 0 0 1,
      V3 1 1 1,
      V3 2 1 1,
      V3 3 0 1,
      V3 0 0 2,
      V3 1 1 2,
      V3 2 1 2,
      V3 3 0 2,
      V3 0 0 3,
      V3 1 0 3,
      V3 2 0 3,
      V3 3 0 3
    ]

surfaceTests :: [TestTree]
surfaceTests =
  [ -- Bezier patch evaluation
    QC.testProperty "bilinear patch corners are correct" $
      approxEqV3 (evalBezierPatch testBilinearPatch 0 0) (V3 0 0 0)
        && approxEqV3 (evalBezierPatch testBilinearPatch 1 0) (V3 1 0 0)
        && approxEqV3 (evalBezierPatch testBilinearPatch 0 1) (V3 0 0 1)
        && approxEqV3 (evalBezierPatch testBilinearPatch 1 1) (V3 1 0 1),
    QC.testProperty "bilinear patch midpoint is average" $
      approxEqV3
        (evalBezierPatch testBilinearPatch 0.5 0.5)
        (V3 0.5 0 0.5),
    -- Bezier patch tessellation
    QC.testProperty "Bezier patch tessellation produces valid mesh" $
      forAll ((,) <$> choose (1, 10) <*> choose (1, 10)) $ \(su, sv) ->
        let m = tessellateBezierPatch testBicubicPatch su sv
         in checkMesh m,
    QC.testProperty "Bezier patch tessellation vertex count" $
      forAll ((,) <$> choose (1, 10) <*> choose (1, 10)) $ \(suRaw, svRaw) ->
        let su = max 1 suRaw
            sv = max 1 svRaw
            m = tessellateBezierPatch testBicubicPatch suRaw svRaw
         in meshVertexCount m == (su + 1) * (sv + 1),
    QC.testProperty "Bezier patch tessellation index count" $
      forAll ((,) <$> choose (1, 10) <*> choose (1, 10)) $ \(suRaw, svRaw) ->
        let su = max 1 suRaw
            sv = max 1 svRaw
            m = tessellateBezierPatch testBicubicPatch suRaw svRaw
         in length (meshIndices m) == 6 * su * sv,
    -- B-spline surface
    QC.testProperty "B-spline surface tessellation produces valid mesh" $
      let surf =
            BSplineSurface
              1
              1
              (knotArray [0, 0, 1, 1])
              (knotArray [0, 0, 1, 1])
              [V3 0 0 0, V3 1 0 0, V3 0 0 1, V3 1 0 1]
       in maybe False checkMesh (tessellateBSplineSurface surf 4 4)
  ]

-- ----------------------------------------------------------------
-- Loft tests
-- ----------------------------------------------------------------

-- | A simple circular profile for revolve testing.
circleProfile :: Float -> V2
circleProfile t = V2 radius height
  where
    radius = 0.5
    height = t * 2.0 - 1.0

-- | Derivative of the simple circle profile.
circleProfileDeriv :: Float -> V2
circleProfileDeriv _ = V2 0 2.0

loftTests :: [TestTree]
loftTests =
  [ -- Revolve
    QC.testProperty "revolve produces valid mesh" $
      let m = revolve circleProfile circleProfileDeriv 8 12 (2 * pi)
       in checkMesh m,
    QC.testProperty "revolve has correct body vertex count" $
      forAll ((,) <$> choose (2, 15) <*> tessParam) $ \(profSegs, slRaw) ->
        let sl = max 3 slRaw
            ps = max 1 profSegs
            m = revolve circleProfile circleProfileDeriv ps sl (2 * pi)
         in meshVertexCount m > 0
              && validIndices m
              && validTriangleCount m,
    -- Revolve with pole (profile touching axis)
    QC.testProperty "revolve with poles produces valid mesh" $
      let poleProfile t = V2 (sin (t * pi)) (cos (t * pi))
          poleDeriv t = V2 (pi * cos (t * pi)) (negate pi * sin (t * pi))
          m = revolve poleProfile poleDeriv 8 12 (2 * pi)
       in checkMesh m,
    -- Loft rings
    QC.testProperty "loftRings rejects fewer than 2 rings" $
      isNothing (loftRings [] False)
        && isNothing (loftRings [[V3 0 0 0, V3 1 0 0, V3 0 1 0]] False),
    QC.testProperty "loftRings rejects rings with fewer than 3 points" $
      isNothing
        ( loftRings
            [[V3 0 0 0, V3 1 0 0], [V3 0 1 0, V3 1 1 0]]
            False
        ),
    QC.testProperty "loftRings produces valid mesh" $
      let ring0 = [V3 (cos t) 0 (sin t) | t <- [0, 2 * pi / 8 .. 2 * pi - 0.01]]
          ring1 = [V3 (cos t) 1 (sin t) | t <- [0, 2 * pi / 8 .. 2 * pi - 0.01]]
          ring2 = [V3 (cos t) 2 (sin t) | t <- [0, 2 * pi / 8 .. 2 * pi - 0.01]]
       in maybe False checkMesh (loftRings [ring0, ring1, ring2] True),
    -- Extrude
    QC.testProperty "extrude produces valid mesh" $
      let profile t = V2 (cos (t * 2 * pi)) (sin (t * 2 * pi))
          deriv t = V2 (negate (2 * pi) * sin (t * 2 * pi)) (2 * pi * cos (t * 2 * pi))
          m = extrude profile deriv (V3 0 1 0) 2.0 12 4
       in checkMesh m,
    -- Sweep
    QC.testProperty "sweep produces valid mesh" $
      let spine t = V3 (t * 4) 0 0
          spineDeriv _ = V3 4 0 0
          profile t = V2 (0.5 * cos (t * 2 * pi)) (0.5 * sin (t * 2 * pi))
          m = sweep spine spineDeriv profile 8 12
       in checkMesh m,
    QC.testProperty "sweep along curved path produces valid mesh" $
      let spine t = V3 (cos (t * pi)) (t * 2) (sin (t * pi))
          spineDeriv t =
            V3
              (negate pi * sin (t * pi))
              2
              (pi * cos (t * pi))
          profile t = V2 (0.3 * cos (t * 2 * pi)) (0.3 * sin (t * 2 * pi))
          m = sweep spine spineDeriv profile 16 8
       in checkMesh m
  ]

-- ----------------------------------------------------------------
-- SDF tests
-- ----------------------------------------------------------------

sdfTests :: [TestTree]
sdfTests =
  [ -- Primitive SDF correctness
    QC.testProperty "sdfSphere is zero on surface" $
      forAll positiveFloat $ \radius ->
        forAll arbitraryUnitV3 $ \dir ->
          let surfacePoint = radius *^ dir
           in approxEq (runSDF (sdfSphere radius) surfacePoint) 0,
    QC.testProperty "sdfSphere is negative inside" $
      forAll positiveFloat $ \radius ->
        runSDF (sdfSphere radius) vzero < 0,
    QC.testProperty "sdfSphere is positive outside" $
      forAll positiveFloat $ \radius ->
        runSDF (sdfSphere radius) (V3 (radius * 2) 0 0) > 0,
    QC.testProperty "sdfBox is zero on face center" $
      forAll positiveFloat $ \halfExtent ->
        let b = V3 halfExtent halfExtent halfExtent
            facePoint = V3 halfExtent 0 0
         in approxEq (runSDF (sdfBox b) facePoint) 0,
    QC.testProperty "sdfBox is negative inside" $
      forAll positiveFloat $ \halfExtent ->
        let b = V3 halfExtent halfExtent halfExtent
         in runSDF (sdfBox b) vzero < 0,
    QC.testProperty "sdfCylinder is zero on barrel surface" $
      forAll positiveFloat $ \radius ->
        forAll positiveFloat $ \halfHeight ->
          let surfacePoint = V3 radius 0 0
           in approxEq (runSDF (sdfCylinder radius halfHeight) surfacePoint) 0,
    QC.testProperty "sdfTorus is zero on outer equator" $
      forAll positiveFloat $ \minor ->
        let major = minor * 2
            outerPoint = V3 (major + minor) 0 0
         in approxEq (runSDF (sdfTorus major minor) outerPoint) 0,
    QC.testProperty "sdfCapsule is zero on surface" $
      forAll positiveFloat $ \radius ->
        let cap = sdfCapsule (V3 0 0 0) (V3 0 2 0) radius
            surfacePoint = V3 radius 1 0
         in approxEq (runSDF cap surfacePoint) 0,
    QC.testProperty "sdfPlane is zero on plane" $
      let pln = sdfPlane (V3 0 1 0) 0
       in approxEq (runSDF pln (V3 5 0 3)) 0,
    -- CSG operations
    QC.testProperty "sdfUnion is min of two SDFs" $
      forAll arbitrary $ \p ->
        let s1 = sdfSphere 1.0
            s2 = sdfTranslate (V3 2 0 0) (sdfSphere 1.0)
            unionDist = runSDF (sdfUnion s1 s2) p
            minDist = min (runSDF s1 p) (runSDF s2 p)
         in approxEq unionDist minDist,
    QC.testProperty "sdfIntersection is max of two SDFs" $
      forAll arbitrary $ \p ->
        let s1 = sdfSphere 1.5
            s2 = sdfBox (V3 1 1 1)
            interDist = runSDF (sdfIntersection s1 s2) p
            maxDist = max (runSDF s1 p) (runSDF s2 p)
         in approxEq interDist maxDist,
    QC.testProperty "sdfDifference subtracts second from first" $
      forAll arbitrary $ \p ->
        let s1 = sdfSphere 2.0
            s2 = sdfSphere 0.5
            diffDist = runSDF (sdfDifference s1 s2) p
            expected = max (runSDF s1 p) (negate (runSDF s2 p))
         in approxEq diffDist expected,
    -- Smooth blending bounds
    QC.testProperty "smoothUnion is bounded by sharp union" $
      forAll arbitrary $ \p ->
        let s1 = sdfSphere 1.0
            s2 = sdfTranslate (V3 1.5 0 0) (sdfSphere 1.0)
            blendK = 0.3
            smoothVal = runSDF (smoothUnion blendK s1 s2) p
            sharp = runSDF (sdfUnion s1 s2) p
         in smoothVal <= sharp + 0.01,
    -- Domain operations
    QC.testProperty "sdfTranslate shifts the field" $
      forAll positiveFloat $ \radius ->
        let offset = V3 3 0 0
            translated = sdfTranslate offset (sdfSphere radius)
         in approxEq (runSDF translated (V3 3 0 0)) (negate radius),
    QC.testProperty "sdfNormal points outward on sphere" $
      forAll positiveFloat $ \radius ->
        forAll arbitraryUnitV3 $ \dir ->
          let s = sdfSphere radius
              surfacePoint = radius *^ dir
              normal = sdfNormal s surfacePoint
           in dot normal dir > 0.9
  ]

-- ----------------------------------------------------------------
-- Isosurface tests
-- ----------------------------------------------------------------

isosurfaceTests :: [TestTree]
isosurfaceTests =
  [ QC.testProperty "marchingCubes on sphere produces valid mesh" $
      let sdf = runSDF (sdfSphere 1.0)
          m = marchingCubes sdf (V3 (-2) (-2) (-2)) (V3 2 2 2) 10 10 10
       in checkMesh m,
    QC.testProperty "marchingCubes sphere has vertices near surface" $
      let radius = 1.0
          sdf = runSDF (sdfSphere radius)
          m = marchingCubes sdf (V3 (-2) (-2) (-2)) (V3 2 2 2) 16 16 16
          vertices = meshVertices m
          maxDeviation = maximum (map (abs . (\v -> vlength (vPosition v) - radius)) vertices)
       in maxDeviation < 0.3,
    QC.testProperty "marchingCubes box produces valid mesh" $
      let sdf = runSDF (sdfBox (V3 1 1 1))
          m = marchingCubes sdf (V3 (-2) (-2) (-2)) (V3 2 2 2) 8 8 8
       in checkMesh m,
    QC.testProperty "marchingCubes empty field produces empty mesh" $
      let sdf _ = 1.0 -- entirely outside
          m = marchingCubes sdf (V3 (-1) (-1) (-1)) (V3 1 1 1) 4 4 4
       in meshVertexCount m == 0 && null (meshIndices m),
    QC.testProperty "marchingCubes full field produces empty mesh" $
      let sdf _ = -1.0 -- entirely inside
          m = marchingCubes sdf (V3 (-1) (-1) (-1)) (V3 1 1 1) 4 4 4
       in meshVertexCount m == 0 && null (meshIndices m),
    QC.testProperty "marchingCubes normals are approximately unit length" $
      let sdf = runSDF (sdfSphere 1.0)
          m = marchingCubes sdf (V3 (-2) (-2) (-2)) (V3 2 2 2) 8 8 8
       in validNormals 0.01 m,
    QC.testProperty "marchingCubes resolution increases vertex count" $
      let sdf = runSDF (sdfSphere 1.0)
          minC = V3 (-2) (-2) (-2)
          maxC = V3 2 2 2
          mLow = marchingCubes sdf minC maxC 4 4 4
          mHigh = marchingCubes sdf minC maxC 12 12 12
       in meshVertexCount mHigh > meshVertexCount mLow,
    QC.testProperty "marchingCubes on CSG union produces valid mesh" $
      let sdf =
            runSDF
              ( sdfUnion
                  (sdfSphere 1.0)
                  (sdfTranslate (V3 1.5 0 0) (sdfSphere 0.8))
              )
          m = marchingCubes sdf (V3 (-2) (-2) (-2)) (V3 4 2 2) 10 8 8
       in checkMesh m
  ]

-- ----------------------------------------------------------------
-- Subdivision tests
-- ----------------------------------------------------------------

subdivisionTests :: [TestTree]
subdivisionTests =
  [ QC.testProperty "subdivide 0 levels is identity" $
      let m = fromMaybe mempty (box 1 1 1 1 1 1)
       in subdivide 0 m == m,
    QC.testProperty "subdivide 1 level produces valid mesh" $
      let m = fromMaybe mempty (box 1 1 1 1 1 1)
          subdiv = subdivide 1 m
       in checkMesh subdiv,
    QC.testProperty "subdivide increases vertex count" $
      let m = fromMaybe mempty (box 1 1 1 1 1 1)
          subdiv = subdivide 1 m
       in meshVertexCount subdiv > meshVertexCount m,
    QC.testProperty "subdivide 2 levels produces valid mesh" $
      let m = fromMaybe mempty (box 1 1 1 1 1 1)
          subdiv = subdivide 2 m
       in checkMesh subdiv,
    QC.testProperty "subdivideLoop 0 levels is identity" $
      let m = fromMaybe mempty (box 1 1 1 1 1 1)
       in subdivideLoop 0 m == m,
    QC.testProperty "subdivideLoop 1 level produces valid mesh" $
      let m = fromMaybe mempty (box 1 1 1 1 1 1)
          subdiv = subdivideLoop 1 m
       in checkMesh subdiv,
    QC.testProperty "subdivideLoop increases vertex count" $
      let m = fromMaybe mempty (box 1 1 1 1 1 1)
          subdiv = subdivideLoop 1 m
       in meshVertexCount subdiv > meshVertexCount m,
    QC.testProperty "subdivideLoop on sphere produces valid mesh" $
      let m = fromMaybe mempty (sphere 1.0 8 6)
          subdiv = subdivideLoop 1 m
       in checkMesh subdiv,
    QC.testProperty "subdivide preserves approximate bounding box" $
      let m = fromMaybe mempty (box 2 2 2 1 1 1)
          subdiv = subdivide 1 m
          positions = map vPosition (meshVertices subdiv)
          maxCoord = maximum (map (\(V3 x y z) -> max (abs x) (max (abs y) (abs z))) positions)
       in maxCoord < 1.5,
    QC.testProperty "subdivideLoop 2 levels produces valid mesh" $
      let m = fromMaybe mempty (box 1 1 1 1 1 1)
          subdiv = subdivideLoop 2 m
       in checkMesh subdiv
  ]

-- ----------------------------------------------------------------
-- Deform tests
-- ----------------------------------------------------------------

deformTests :: [TestTree]
deformTests =
  [ QC.testProperty "twist by 0 is identity" $
      let m = fromMaybe mempty (cylinder 1 2 8 4 True True)
          twisted = twist (V3 0 1 0) 0.0 m
       in all
            (\(v1, v2) -> approxEqV3 (vPosition v1) (vPosition v2))
            (zip (meshVertices m) (meshVertices twisted)),
    QC.testProperty "twist preserves vertex count" $
      let m = fromMaybe mempty (cylinder 1 2 8 4 True True)
          twisted = twist (V3 0 1 0) 1.0 m
       in meshVertexCount twisted == meshVertexCount m,
    QC.testProperty "twist preserves index validity" $
      let m = fromMaybe mempty (cylinder 1 2 8 4 True True)
          twisted = twist (V3 0 1 0) 1.0 m
       in validIndices twisted && validTriangleCount twisted,
    QC.testProperty "bend by 0 is identity" $
      let m = fromMaybe mempty (cylinder 1 2 8 4 True True)
          bent = bend (V3 0 1 0) 0.0 m
       in all
            (\(v1, v2) -> approxEqV3 (vPosition v1) (vPosition v2))
            (zip (meshVertices m) (meshVertices bent)),
    QC.testProperty "bend preserves vertex count" $
      let m = fromMaybe mempty (cylinder 1 2 8 4 True True)
          bent = bend (V3 0 1 0) 0.5 m
       in meshVertexCount bent == meshVertexCount m,
    QC.testProperty "taper with uniform scale is identity" $
      let m = fromMaybe mempty (cylinder 1 2 8 4 True True)
          tapered = taper (V3 0 1 0) 1.0 1.0 m
       in all
            (\(v1, v2) -> approxEqV3 (vPosition v1) (vPosition v2))
            (zip (meshVertices m) (meshVertices tapered)),
    QC.testProperty "taper preserves vertex count" $
      let m = fromMaybe mempty (cylinder 1 2 8 4 True True)
          tapered = taper (V3 0 1 0) 1.0 0.5 m
       in meshVertexCount tapered == meshVertexCount m,
    QC.testProperty "taper narrows the mesh at one end" $
      let m = fromMaybe mempty (cylinder 1 2 8 4 False False)
          tapered = taper (V3 0 1 0) 1.0 0.0 m
          topPositions =
            [ vPosition v
            | v <- meshVertices tapered,
              let V3 _ py _ = vPosition v,
              py > 0.9
            ]
          topRadii = map (\(V3 px _ pz) -> sqrt (px * px + pz * pz)) topPositions
       in all (< 0.1) topRadii,
    QC.testProperty "defaultLattice FFD is identity" $
      let m = fromMaybe mempty (box 1 1 1 1 1 1)
          lattice = defaultLattice (V3 (-1) (-1) (-1)) (V3 2 2 2) 2 2 2
          deformed = ffd lattice m
       in all
            (\(v1, v2) -> approxEqV3 (vPosition v1) (vPosition v2))
            (zip (meshVertices m) (meshVertices deformed)),
    QC.testProperty "FFD preserves vertex count" $
      let m = fromMaybe mempty (box 1 1 1 1 1 1)
          lattice = defaultLattice (V3 (-1) (-1) (-1)) (V3 2 2 2) 3 3 3
          deformed = ffd lattice m
       in meshVertexCount deformed == meshVertexCount m,
    QC.testProperty "displace by zero is identity" $
      let m = fromMaybe mempty (sphere 1.0 8 6)
          displaced = displace (const 0) m
       in all
            (\(v1, v2) -> approxEqV3 (vPosition v1) (vPosition v2))
            (zip (meshVertices m) (meshVertices displaced)),
    QC.testProperty "displace preserves vertex count" $
      let m = fromMaybe mempty (sphere 1.0 8 6)
          displaced = displace (const 0.1) m
       in meshVertexCount displaced == meshVertexCount m,
    QC.testProperty "displace outward increases bounding radius" $
      let m = fromMaybe mempty (sphere 1.0 12 8)
          displaced = displace (const 0.5) m
          origMax = maximum (map (vlength . vPosition) (meshVertices m))
          dispMax = maximum (map (vlength . vPosition) (meshVertices displaced))
       in dispMax > origMax
  ]

-- ----------------------------------------------------------------
-- Noise tests
-- ----------------------------------------------------------------

noiseTests :: [TestTree]
noiseTests =
  [ -- Perlin 2D
    QC.testProperty "perlin2D is bounded" $
      forAll (choose (0, 10000) :: Gen Word64) $ \seed ->
        forAll smallFloat $ \px ->
          forAll smallFloat $ \py ->
            let config = mkNoiseConfig seed
                val = perlin2D config px py
             in val >= -2.0 && val <= 2.0,
    QC.testProperty "perlin2D is deterministic" $
      forAll (choose (0, 10000) :: Gen Word64) $ \seed ->
        let config = mkNoiseConfig seed
         in perlin2D config 1.5 2.7 == perlin2D config 1.5 2.7,
    QC.testProperty "perlin2D different seeds produce different fields" $
      let c1 = mkNoiseConfig 42
          c2 = mkNoiseConfig 137
          samples = [(fromIntegral x * 0.7, fromIntegral y * 0.7) | x <- [0 :: Int .. 3], y <- [0 :: Int .. 3]]
          vals1 = map (uncurry (perlin2D c1)) samples
          vals2 = map (uncurry (perlin2D c2)) samples
       in vals1 /= vals2,
    -- Perlin 3D
    QC.testProperty "perlin3D is bounded" $
      forAll (choose (0, 10000) :: Gen Word64) $ \seed ->
        forAll smallFloat $ \px ->
          forAll smallFloat $ \py ->
            forAll smallFloat $ \pz ->
              let config = mkNoiseConfig seed
                  val = perlin3D config px py pz
               in val >= -2.0 && val <= 2.0,
    QC.testProperty "perlin3D is deterministic" $
      forAll (choose (0, 10000) :: Gen Word64) $ \seed ->
        let config = mkNoiseConfig seed
         in perlin3D config 0.5 1.5 2.5 == perlin3D config 0.5 1.5 2.5,
    -- Simplex 2D
    QC.testProperty "simplex2D is bounded" $
      forAll (choose (0, 10000) :: Gen Word64) $ \seed ->
        forAll smallFloat $ \px ->
          forAll smallFloat $ \py ->
            let config = mkNoiseConfig seed
                val = simplex2D config px py
             in val >= -2.0 && val <= 2.0,
    QC.testProperty "simplex2D is deterministic" $
      forAll (choose (0, 10000) :: Gen Word64) $ \seed ->
        let config = mkNoiseConfig seed
         in simplex2D config 3.14 2.72 == simplex2D config 3.14 2.72,
    -- Simplex 3D
    QC.testProperty "simplex3D is bounded" $
      forAll (choose (0, 10000) :: Gen Word64) $ \seed ->
        forAll smallFloat $ \px ->
          forAll smallFloat $ \py ->
            forAll smallFloat $ \pz ->
              let config = mkNoiseConfig seed
                  val = simplex3D config px py pz
               in val >= -2.0 && val <= 2.0,
    -- Worley 2D
    QC.testProperty "worley2D F1 is non-negative" $
      forAll (choose (0, 10000) :: Gen Word64) $ \seed ->
        forAll smallFloat $ \px ->
          forAll smallFloat $ \py ->
            let config = mkNoiseConfig seed
                result = worley2D config px py
             in worleyF1 result >= 0,
    QC.testProperty "worley2D F1 <= F2" $
      forAll (choose (0, 10000) :: Gen Word64) $ \seed ->
        forAll smallFloat $ \px ->
          forAll smallFloat $ \py ->
            let config = mkNoiseConfig seed
                result = worley2D config px py
             in worleyF1 result <= worleyF2 result + 1.0e-6,
    QC.testProperty "worley2D is deterministic" $
      forAll (choose (0, 10000) :: Gen Word64) $ \seed ->
        let config = mkNoiseConfig seed
            r1 = worley2D config 1.5 2.5
            r2 = worley2D config 1.5 2.5
         in worleyF1 r1 == worleyF1 r2 && worleyF2 r1 == worleyF2 r2,
    -- Worley 3D
    QC.testProperty "worley3D F1 is non-negative" $
      forAll (choose (0, 10000) :: Gen Word64) $ \seed ->
        forAll smallFloat $ \px ->
          forAll smallFloat $ \py ->
            forAll smallFloat $ \pz ->
              let config = mkNoiseConfig seed
                  result = worley3D config px py pz
               in worleyF1 result >= 0,
    QC.testProperty "worley3D F1 <= F2" $
      forAll (choose (0, 10000) :: Gen Word64) $ \seed ->
        forAll smallFloat $ \px ->
          forAll smallFloat $ \py ->
            forAll smallFloat $ \pz ->
              let config = mkNoiseConfig seed
                  result = worley3D config px py pz
               in worleyF1 result <= worleyF2 result + 1.0e-6,
    -- FBM
    QC.testProperty "fbm is bounded" $
      forAll (choose (0, 10000) :: Gen Word64) $ \seed ->
        forAll smallFloat $ \px ->
          forAll smallFloat $ \py ->
            let config = mkNoiseConfig seed
                val = fbm (perlin2D config) 4 defaultLacunarity defaultPersistence px py
             in val >= -2.0 && val <= 2.0,
    QC.testProperty "fbm3D is bounded" $
      forAll (choose (0, 10000) :: Gen Word64) $ \seed ->
        forAll smallFloat $ \px ->
          forAll smallFloat $ \py ->
            forAll smallFloat $ \pz ->
              let config = mkNoiseConfig seed
                  val = fbm3D (perlin3D config) 4 defaultLacunarity defaultPersistence px py pz
               in val >= -2.0 && val <= 2.0,
    -- Ridged
    QC.testProperty "ridged is non-negative" $
      forAll (choose (0, 10000) :: Gen Word64) $ \seed ->
        forAll smallFloat $ \px ->
          forAll smallFloat $ \py ->
            let config = mkNoiseConfig seed
                val = ridged (perlin2D config) 4 defaultLacunarity defaultPersistence px py
             in val >= -0.01,
    -- Turbulence
    QC.testProperty "turbulence is non-negative" $
      forAll (choose (0, 10000) :: Gen Word64) $ \seed ->
        forAll smallFloat $ \px ->
          forAll smallFloat $ \py ->
            let config = mkNoiseConfig seed
                val = turbulence (perlin2D config) 4 defaultLacunarity defaultPersistence px py
             in val >= -0.01,
    -- Domain warp
    QC.testProperty "domainWarp2D with zero amplitude is base noise" $
      forAll (choose (0, 10000) :: Gen Word64) $ \seed ->
        forAll smallFloat $ \px ->
          forAll smallFloat $ \py ->
            let config = mkNoiseConfig seed
                base = perlin2D config px py
                warped = domainWarp2D (perlin2D config) 0 px py
             in approxEq base warped
  ]

-- ----------------------------------------------------------------
-- Skeleton tests
-- ----------------------------------------------------------------

skeletonTests :: [TestTree]
skeletonTests =
  [ QC.testProperty "mkSkeleton rejects empty list" $
      isNothing (mkSkeleton []),
    QC.testProperty "mkSkeleton rejects no root" $
      isNothing (mkSkeleton [Joint 0 1 vzero, Joint 1 0 vzero]),
    QC.testProperty "mkSkeleton rejects multiple roots" $
      isNothing (mkSkeleton [Joint 0 (-1) vzero, Joint 1 (-1) vzero]),
    QC.testProperty "mkSkeleton rejects duplicate IDs" $
      isNothing (mkSkeleton [Joint 0 (-1) vzero, Joint 0 (-1) vzero]),
    QC.testProperty "mkSkeleton rejects missing parent" $
      isNothing (mkSkeleton [Joint 0 (-1) vzero, Joint 1 99 vzero]),
    QC.testProperty "mkSkeleton accepts valid single joint" $
      isJust (mkSkeleton [Joint 0 (-1) vzero]),
    QC.testProperty "mkSkeleton accepts valid chain" $
      isJust (mkSkeleton [Joint 0 (-1) vzero, Joint 1 0 (V3 0 1 0), Joint 2 1 (V3 0 1 0)]),
    QC.testProperty "skelRoot returns root ID" $
      case mkSkeleton [Joint 5 (-1) vzero, Joint 6 5 (V3 1 0 0)] of
        Just skel -> skelRoot skel == 5
        Nothing -> False,
    QC.testProperty "skelChildren returns correct children" $
      case mkSkeleton [Joint 0 (-1) vzero, Joint 1 0 (V3 1 0 0), Joint 2 0 (V3 0 1 0)] of
        Just skel -> length (skelChildren skel 0) == 2
        Nothing -> False,
    QC.testProperty "skelBones returns all non-root edges" $
      case mkSkeleton [Joint 0 (-1) vzero, Joint 1 0 (V3 1 0 0), Joint 2 1 (V3 0 1 0)] of
        Just skel -> length (skelBones skel) == 2
        Nothing -> False,
    QC.testProperty "skelJointCount matches input" $
      case mkSkeleton [Joint 0 (-1) vzero, Joint 1 0 (V3 1 0 0), Joint 2 1 (V3 0 1 0)] of
        Just skel -> skelJointCount skel == 3
        Nothing -> False,
    QC.testProperty "skelRestPositions root is at local offset" $
      case mkSkeleton [Joint 0 (-1) (V3 0 5 0)] of
        Just skel ->
          let positions = skelRestPositions skel
           in approxEqV3 (IntMap.findWithDefault vzero 0 positions) (V3 0 5 0)
        Nothing -> False,
    QC.testProperty "skelRestPositions propagates offsets" $
      case mkSkeleton [Joint 0 (-1) (V3 0 0 0), Joint 1 0 (V3 0 2 0), Joint 2 1 (V3 0 3 0)] of
        Just skel ->
          let positions = skelRestPositions skel
           in approxEqV3 (IntMap.findWithDefault vzero 2 positions) (V3 0 5 0)
        Nothing -> False,
    QC.testProperty "humanoid produces valid skeleton" $
      forAll positiveFloat $ \height ->
        isJust (humanoid height),
    QC.testProperty "humanoid has 17 joints" $
      case humanoid 1.8 of
        Just skel -> skelJointCount skel == 17
        Nothing -> False,
    QC.testProperty "humanoid rejects non-positive height" $
      isNothing (humanoid 0) && isNothing (humanoid (-1)),
    QC.testProperty "quadruped produces valid skeleton" $
      forAll positiveFloat $ \bodyLen ->
        forAll positiveFloat $ \height ->
          isJust (quadruped bodyLen height),
    QC.testProperty "quadruped has 17 joints" $
      case quadruped 2.0 1.2 of
        Just skel -> skelJointCount skel == 17
        Nothing -> False
  ]

-- ----------------------------------------------------------------
-- Pose tests
-- ----------------------------------------------------------------

poseTests :: [TestTree]
poseTests =
  [ QC.testProperty "restPose applyPose matches skelRestPositions" $
      case humanoid 1.8 of
        Just skel ->
          let restPositions = skelRestPositions skel
              posePositions = applyPose skel restPose
           in all
                (\jid -> approxEqV3 (lk jid restPositions) (lk jid posePositions))
                (IntMap.keys (skelJoints skel))
        Nothing -> False,
    QC.testProperty "applyPose preserves root position" $
      case mkSkeleton [Joint 0 (-1) (V3 0 1 0), Joint 1 0 (V3 0 1 0)] of
        Just skel ->
          forAll arbitrary $ \q ->
            let positions = applyPose skel (singleJoint 1 q)
             in approxEqV3 (lk 0 positions) (V3 0 1 0)
        Nothing -> property False,
    QC.testProperty "applyPose rotates child around parent" $
      case mkSkeleton [Joint 0 (-1) vzero, Joint 1 0 (V3 1 0 0)] of
        Just skel ->
          let rot90Y = axisAngle (V3 0 1 0) (pi / 2)
              positions = applyPose skel (singleJoint 0 rot90Y)
              childPos = lk 1 positions
           in approxEqV3 childPos (V3 0 0 (-1))
        Nothing -> False,
    QC.testProperty "lerpPose at 0 gives first pose" $
      let pA = singleJoint 0 (axisAngle (V3 0 1 0) 0.5)
          pB = singleJoint 0 (axisAngle (V3 0 1 0) 1.5)
          interpolated = lerpPose 0 pA pB
          Quaternion w1 _ = IntMap.findWithDefault (Quaternion 1 vzero) 0 (unPose pA)
          Quaternion w2 _ = IntMap.findWithDefault (Quaternion 1 vzero) 0 (unPose interpolated)
       in approxEq w1 w2,
    QC.testProperty "lerpPose at 1 gives second pose" $
      let pA = singleJoint 0 (axisAngle (V3 0 1 0) 0.5)
          pB = singleJoint 0 (axisAngle (V3 0 1 0) 1.5)
          interpolated = lerpPose 1 pA pB
          Quaternion w1 _ = IntMap.findWithDefault (Quaternion 1 vzero) 0 (unPose pB)
          Quaternion w2 _ = IntMap.findWithDefault (Quaternion 1 vzero) 0 (unPose interpolated)
       in approxEq w1 w2,
    QC.testProperty "slerpQuat preserves unit length" $
      forAll arbitrary $ \q1 ->
        forAll arbitrary $ \q2 ->
          forAll (choose (0, 1)) $ \t ->
            let Quaternion w (V3 x y z) = slerpQuat t q1 q2
                len = sqrt (w * w + x * x + y * y + z * z)
             in approxEq len 1.0,
    QC.testProperty "slerpQuat at 0 returns first quaternion" $
      forAll arbitrary $ \q ->
        let Quaternion w1 (V3 x1 y1 z1) = q
            Quaternion w2 (V3 x2 y2 z2) = slerpQuat 0 q (axisAngle (V3 0 1 0) 1.0)
         in approxEq (abs (w1 * w2 + x1 * x2 + y1 * y2 + z1 * z2)) 1.0
  ]
  where
    lk = IntMap.findWithDefault vzero

-- ----------------------------------------------------------------
-- Animate tests
-- ----------------------------------------------------------------

animateTests :: [TestTree]
animateTests =
  [ -- Building blocks
    QC.testProperty "oscillate produces single-joint pose" $
      forAll positiveFloat $ \t ->
        let pose = oscillate 5 (V3 1 0 0) 0.3 1.0 t
         in IntMap.size (unPose pose) == 1 && IntMap.member 5 (unPose pose),
    QC.testProperty "oscillate at t=0 is identity rotation" $
      let pose = oscillate 0 (V3 1 0 0) 0.5 1.0 0
          Quaternion w _ = IntMap.findWithDefault (Quaternion 1 vzero) 0 (unPose pose)
       in approxEq w 1.0,
    QC.testProperty "oscillatePositive never produces negative rotation" $
      forAll (choose (0, 10)) $ \t ->
        let pose = oscillatePositive 0 (V3 1 0 0) 1.0 1.0 t
            Quaternion _ (V3 x _ _) = IntMap.findWithDefault (Quaternion 1 vzero) 0 (unPose pose)
         in x >= -0.01,
    QC.testProperty "combine merges multiple joints" $
      forAll positiveFloat $ \t ->
        let anim =
              combine
                [ oscillate 0 (V3 1 0 0) 0.3 1.0,
                  oscillate 5 (V3 0 1 0) 0.2 1.0,
                  oscillate 11 (V3 0 0 1) 0.4 1.0
                ]
            pose = anim t
         in IntMap.size (unPose pose) == 3,
    QC.testProperty "delay shifts phase" $
      let original = oscillate 0 (V3 1 0 0) 0.5 2.0
          delayed = delay 1.0 original
          poseOrig = original 0.5
          poseDelayed = delayed 1.5
       in approxEqPose poseOrig poseDelayed,
    QC.testProperty "timeScale 2 doubles speed" $
      let original = oscillate 0 (V3 1 0 0) 0.5 2.0
          fast = timeScale 2.0 original
          poseOrig = original 1.0
          poseFast = fast 0.5
       in approxEqPose poseOrig poseFast,
    -- Composition
    QC.testProperty "blendAnimations at 0 gives first animation" $
      let animA = oscillate 0 (V3 1 0 0) 0.3 1.0
          animB = oscillate 0 (V3 0 1 0) 0.6 1.0
          blended = blendAnimations 0 animA animB
       in approxEqPose (blended 0.25) (animA 0.25),
    QC.testProperty "loopAnimation wraps time" $
      let anim = loopAnimation 1.0 (oscillate 0 (V3 1 0 0) 0.5 1.0)
       in approxEqPose (anim 0.3) (anim 1.3),
    QC.testProperty "constantPose always returns same pose" $
      let pose = singleJoint 0 (axisAngle (V3 0 1 0) 0.5)
          anim = constantPose pose
       in anim 0.0 == pose && anim 99.9 == pose,
    QC.testProperty "sequenceAnimations evaluates correct segment" $
      let seg1 = constantPose (singleJoint 0 (axisAngle (V3 1 0 0) 0.1))
          seg2 = constantPose (singleJoint 0 (axisAngle (V3 0 1 0) 0.2))
          anim = sequenceAnimations [(1.0, seg1), (1.0, seg2)]
       in anim 0.5 == singleJoint 0 (axisAngle (V3 1 0 0) 0.1),
    -- Integration: generic animation on any skeleton
    QC.testProperty "combine + applyPose works on humanoid" $
      case humanoid 1.8 of
        Just skel ->
          forAll (choose (0, 10)) $ \t ->
            let anim =
                  combine
                    [ oscillate 11 (V3 1 0 0) 0.4 1.0,
                      oscillate 14 (V3 1 0 0) (-0.4) 1.0
                    ]
                positions = applyPose skel (anim t)
             in IntMap.size positions == skelJointCount skel
        Nothing -> property False,
    QC.testProperty "combine + applyPose works on quadruped" $
      case quadruped 2.0 1.2 of
        Just skel ->
          forAll (choose (0, 10)) $ \t ->
            let anim =
                  combine
                    [ oscillate 5 (V3 1 0 0) 0.3 1.0,
                      oscillate 8 (V3 1 0 0) (-0.3) 1.0,
                      oscillate 11 (V3 1 0 0) (-0.3) 1.0,
                      oscillate 14 (V3 1 0 0) 0.3 1.0
                    ]
                positions = applyPose skel (anim t)
             in IntMap.size positions == skelJointCount skel
        Nothing -> property False
  ]

-- ----------------------------------------------------------------
-- IK tests
-- ----------------------------------------------------------------

ikTests :: [TestTree]
ikTests =
  [ QC.testProperty "CCD moves end effector closer to target" $
      once $
        case mkSkeleton
          [ Joint 0 (-1) vzero,
            Joint 1 0 (V3 0 1 0),
            Joint 2 1 (V3 0 1 0),
            Joint 3 2 (V3 0 1 0)
          ] of
          Just skel ->
            let target = V3 0 3 0
                result = solveCCD skel restPose [0, 1, 2, 3] target 100
                origPositions = applyPose skel restPose
                newPositions = applyPose skel result
                origEffector = IntMap.findWithDefault vzero 3 origPositions
                newEffector = IntMap.findWithDefault vzero 3 newPositions
                origDist = vlength (target ^-^ origEffector)
                newDist = vlength (target ^-^ newEffector)
             in newDist <= origDist + 0.01
          Nothing -> False,
    QC.testProperty "FABRIK moves end effector closer to target" $
      once $
        case mkSkeleton
          [ Joint 0 (-1) vzero,
            Joint 1 0 (V3 0 1 0),
            Joint 2 1 (V3 0 1 0),
            Joint 3 2 (V3 0 1 0)
          ] of
          Just skel ->
            let target = V3 0 3 0
                result = solveFABRIK skel restPose [0, 1, 2, 3] target 0.1 100
                origPositions = applyPose skel restPose
                newPositions = applyPose skel result
                origEffector = IntMap.findWithDefault vzero 3 origPositions
                newEffector = IntMap.findWithDefault vzero 3 newPositions
                origDist = vlength (target ^-^ origEffector)
                newDist = vlength (target ^-^ newEffector)
             in newDist <= origDist + 0.01
          Nothing -> False,
    QC.testProperty "fabrikReachable returns False for far target" $
      once $
        case mkSkeleton
          [ Joint 0 (-1) vzero,
            Joint 1 0 (V3 0 1 0),
            Joint 2 1 (V3 0 1 0)
          ] of
          Just skel ->
            not (fabrikReachable skel restPose [0, 1, 2] (V3 100 0 0))
          Nothing -> False,
    QC.testProperty "fabrikReachable returns True for close target" $
      once $
        case mkSkeleton
          [ Joint 0 (-1) vzero,
            Joint 1 0 (V3 0 1 0),
            Joint 2 1 (V3 0 1 0)
          ] of
          Just skel ->
            fabrikReachable skel restPose [0, 1, 2] (V3 0 1.5 0)
          Nothing -> False
  ]

-- ----------------------------------------------------------------
-- Skin tests
-- ----------------------------------------------------------------

skinTests :: [TestTree]
skinTests =
  [ QC.testProperty "identity pose leaves mesh approximately unchanged" $
      once $
        case (humanoid 1.8, sphere 0.5 8 4) of
          (Just skel, Just mesh) ->
            let binding = buildSkinBinding skel mesh 10.0
                skinned = applySkin skel restPose binding mesh
                origPositions = map vPosition (meshVertices mesh)
                newPositions = map vPosition (meshVertices skinned)
                -- With rest pose, positions should stay close
                maxDrift =
                  maximum
                    ( zipWith
                        (\a b -> vlength (a ^-^ b))
                        origPositions
                        newPositions
                    )
             in maxDrift < 2.0
          _ -> False,
    QC.testProperty "buildSkinBinding produces valid weights" $
      once $
        case (humanoid 1.8, sphere 0.5 8 4) of
          (Just skel, Just mesh) ->
            let binding = buildSkinBinding skel mesh 10.0
                allVerts = skinWeights binding
                allWeightsNonNeg =
                  all
                    (all (\bw -> bwWeight bw >= 0) . svWeights)
                    allVerts
                maxInfluencesOk =
                  all
                    (\sv -> length (svWeights sv) <= maxInfluences)
                    allVerts
             in allWeightsNonNeg && maxInfluencesOk
          _ -> False
  ]

-- ----------------------------------------------------------------
-- Morph tests
-- ----------------------------------------------------------------

morphTests :: [TestTree]
morphTests =
  [ QC.testProperty "morphMesh at t=0 returns first mesh positions" $
      once $
        case (box 1 1 1 1 1 1, box 2 2 2 1 1 1) of
          (Just meshA, Just meshB) ->
            let morphed = morphMesh 0 meshA meshB
                origPositions = map vPosition (meshVertices meshA)
                morphPositions' = map vPosition (meshVertices morphed)
             in all (uncurry approxEqV3) (zip origPositions morphPositions')
          _ -> False,
    QC.testProperty "morphMesh at t=1 returns second mesh positions" $
      once $
        case (box 1 1 1 1 1 1, box 2 2 2 1 1 1) of
          (Just meshA, Just meshB) ->
            let morphed = morphMesh 1 meshA meshB
                targetPositions = map vPosition (meshVertices meshB)
                morphPositions' = map vPosition (meshVertices morphed)
             in all (uncurry approxEqV3) (zip targetPositions morphPositions')
          _ -> False,
    QC.testProperty "blendShapes with zero weights returns base mesh" $
      once $
        case (box 1 1 1 1 1 1, box 2 2 2 1 1 1) of
          (Just baseMesh, Just targetMesh) ->
            let result = blendShapes baseMesh [(0.0, targetMesh)]
                basePositions = map vPosition (meshVertices baseMesh)
                resultPositions = map vPosition (meshVertices result)
             in all (uncurry approxEqV3) (zip basePositions resultPositions)
          _ -> False
  ]

-- ----------------------------------------------------------------
-- DualContour tests
-- ----------------------------------------------------------------

dualContourTests :: [TestTree]
dualContourTests =
  [ QC.testProperty "sphere SDF produces non-empty mesh" $
      once $
        let sdf = runSDF (sdfSphere 1.0)
            m = dualContour sdf (V3 (-2) (-2) (-2)) (V3 2 2 2) 10 10 10
         in meshVertexCount m > 0 && not (null (meshIndices m)),
    QC.testProperty "sphere SDF produces valid indices" $
      once $
        let sdf = runSDF (sdfSphere 1.0)
            m = dualContour sdf (V3 (-2) (-2) (-2)) (V3 2 2 2) 10 10 10
         in validIndices m
  ]

-- ----------------------------------------------------------------
-- Hull tests
-- ----------------------------------------------------------------

hullTests :: [TestTree]
hullTests =
  [ QC.testProperty "tetrahedron from 4 non-coplanar points" $
      once $
        let pts =
              [ V3 0 0 0,
                V3 1 0 0,
                V3 0 1 0,
                V3 0 0 1
              ]
         in case convexHull pts of
              Just m -> length (meshIndices m) == 12
              Nothing -> False,
    QC.testProperty "coplanar points return Nothing" $
      once $
        let pts =
              [ V3 0 0 0,
                V3 1 0 0,
                V3 0 1 0,
                V3 1 1 0
              ]
         in isNothing (convexHull pts)
  ]

-- ----------------------------------------------------------------
-- Simplify tests
-- ----------------------------------------------------------------

simplifyTests :: [TestTree]
simplifyTests =
  [ QC.testProperty "simplification reduces triangle count" $
      once $
        case sphere 1.0 12 8 of
          Just m ->
            let trisBefore = length (meshIndices m) `div` 3
                simplified = simplify (trisBefore `div` 2) m
                trisAfter = length (meshIndices simplified) `div` 3
             in trisAfter < trisBefore
          Nothing -> False,
    QC.testProperty "simplified mesh has valid indices" $
      once $
        case sphere 1.0 12 8 of
          Just m ->
            let trisBefore = length (meshIndices m) `div` 3
                simplified = simplify (trisBefore `div` 2) m
             in validIndices simplified
          Nothing -> False
  ]

-- ----------------------------------------------------------------
-- Smooth tests
-- ----------------------------------------------------------------

smoothTests :: [TestTree]
smoothTests =
  [ QC.testProperty "zero iterations returns original vertex positions" $
      once $
        case box 1 1 1 1 1 1 of
          Just m ->
            let smoothed = smooth 0 0.5 m
                origPos = map vPosition (meshVertices m)
                smoothPos = map vPosition (meshVertices smoothed)
             in all (uncurry approxEqV3) (zip origPos smoothPos)
          Nothing -> False,
    QC.testProperty "smoothing produces valid mesh" $
      once $
        case sphere 1.0 8 6 of
          Just m ->
            let smoothed = smooth 2 0.5 m
             in meshVertexCount smoothed > 0
                  && not (null (meshIndices smoothed))
                  && validIndices smoothed
          Nothing -> False
  ]

-- ----------------------------------------------------------------
-- Weld tests
-- ----------------------------------------------------------------

weldTests :: [TestTree]
weldTests =
  [ QC.testProperty "duplicate vertices are merged" $
      once $
        let v0 = vertex (V3 0 0 0) (V3 0 1 0) (V2 0 0) (V4 1 0 0 1)
            v1 = vertex (V3 1 0 0) (V3 0 1 0) (V2 1 0) (V4 1 0 0 1)
            v2 = vertex (V3 0 1 0) (V3 0 1 0) (V2 0 1) (V4 1 0 0 1)
            -- Duplicate of v0
            v3 = vertex (V3 0 0 0) (V3 0 1 0) (V2 0 0) (V4 1 0 0 1)
            m = mkMesh [v0, v1, v2, v3] [0, 1, 2, 3, 1, 2]
            welded = weldVertices 0.001 m
         in meshVertexCount welded < meshVertexCount m,
    QC.testProperty "welded mesh has valid indices" $
      once $
        let v0 = vertex (V3 0 0 0) (V3 0 1 0) (V2 0 0) (V4 1 0 0 1)
            v1 = vertex (V3 1 0 0) (V3 0 1 0) (V2 1 0) (V4 1 0 0 1)
            v2 = vertex (V3 0 1 0) (V3 0 1 0) (V2 0 1) (V4 1 0 0 1)
            v3 = vertex (V3 0 0 0) (V3 0 1 0) (V2 0 0) (V4 1 0 0 1)
            m = mkMesh [v0, v1, v2, v3] [0, 1, 2, 3, 1, 2]
            welded = weldVertices 0.001 m
         in validIndices welded
  ]

-- ----------------------------------------------------------------
-- Icosphere tests
-- ----------------------------------------------------------------

icosphereTests :: [TestTree]
icosphereTests =
  [ QC.testProperty "level 0 produces 20 triangles" $
      once $
        case icosphere 1.0 0 of
          Just m -> length (meshIndices m) `div` 3 == 20
          Nothing -> False,
    QC.testProperty "all vertices at correct radius" $
      once $
        let radius = 2.5
         in case icosphere radius 1 of
              Just m ->
                all
                  ( \v ->
                      let dist = vlength (vPosition v)
                       in abs (dist - radius) < 0.01
                  )
                  (meshVertices m)
              Nothing -> False
  ]

-- ----------------------------------------------------------------
-- Export tests
-- ----------------------------------------------------------------

exportTests :: [TestTree]
exportTests =
  [ QC.testProperty "OBJ export contains vertex lines" $
      once $
        case box 1 1 1 1 1 1 of
          Just m ->
            let obj = meshToOBJ m
             in "v " `isInfixOf` obj
          Nothing -> False,
    QC.testProperty "glTF export is non-empty" $
      once $
        case box 1 1 1 1 1 1 of
          Just m ->
            let gltf = meshToGLTF m
             in not (null gltf)
          Nothing -> False
  ]

-- ----------------------------------------------------------------
-- Terrain tests
-- ----------------------------------------------------------------

terrainTests :: [TestTree]
terrainTests =
  [ QC.testProperty "terrain generates valid mesh" $
      once $
        case terrain (const (const 0.0)) 10 10 4 4 of
          Just m ->
            validIndices m
              && validTriangleCount m
              && meshVertexCount m == 25
          Nothing -> False,
    QC.testProperty "terrain with height function has correct vertex count" $
      once $
        case terrain (\x z -> sin x * cos z) 5 5 8 8 of
          Just m -> meshVertexCount m == 81
          Nothing -> False,
    QC.testProperty "fromHeightmap produces valid mesh" $
      once $
        let grid = [[0, 1, 0], [1, 2, 1], [0, 1, 0]]
         in case fromHeightmap 3.0 3.0 grid of
              Just m -> validIndices m && meshVertexCount m == 9
              Nothing -> False,
    QC.testProperty "sampleGrid has correct dimensions" $
      once $
        let grid = sampleGrid (const (const 1.0)) 3 4 10 10
         in length grid == 4 && all (\row -> length row == 5) grid,
    QC.testProperty "terrace quantizes heights" $
      once $
        let grid = [[0.1, 0.5, 0.9]]
            terraced = terrace 3 grid
         in all (all (\h -> h >= 0 && h <= 1.0)) terraced,
    QC.testProperty "clampHeights respects bounds" $
      once $
        let grid = [[-5, 0, 5, 10]]
            clamped = clampHeights 0 5 grid
         in clamped == [[0, 0, 5, 5]],
    QC.testProperty "thermalErosion preserves grid dimensions" $
      once $
        let grid = [[1, 5, 1], [5, 10, 5], [1, 5, 1]]
            eroded = thermalErosion 5 0.5 grid
         in length eroded == 3 && all (\row -> length row == 3) eroded,
    QC.testProperty "hydraulicErosion preserves grid dimensions" $
      once $
        let grid = [[1, 5, 1], [5, 10, 5], [1, 5, 1]]
            eroded = hydraulicErosion 3 0.01 0.1 grid
         in length eroded == 3 && all (\row -> length row == 3) eroded,
    QC.testProperty "terrain rejects non-positive dimensions" $
      once $
        isNothing (terrain (const (const 0)) 0 10 4 4)
          && isNothing (terrain (const (const 0)) 10 0 4 4)
  ]

-- ----------------------------------------------------------------
-- UV tests
-- ----------------------------------------------------------------

uvTests :: [TestTree]
uvTests =
  [ QC.testProperty "projectPlanar XZ produces valid UVs" $
      once $
        case plane 2 2 2 2 of
          Just m ->
            let projected = projectPlanar XZPlane 1.0 m
             in meshVertexCount projected == meshVertexCount m
          Nothing -> False,
    QC.testProperty "scaleUV doubles coordinates" $
      once $
        let v0 = vertex (V3 0 0 0) (V3 0 1 0) (V2 0.5 0.25) (V4 1 0 0 1)
            m = mkMesh [v0] []
            scaled = scaleUV 2.0 2.0 m
            V2 u _v = case meshVertices scaled of
              (vtx : _) -> vUV vtx
              [] -> V2 0 0
         in approxEq u 1.0,
    QC.testProperty "rotateUV by 0 preserves coordinates" $
      once $
        let v0 = vertex (V3 0 0 0) (V3 0 1 0) (V2 0.3 0.7) (V4 1 0 0 1)
            m = mkMesh [v0] []
            rotated = rotateUV 0 m
            V2 u v = case meshVertices rotated of
              (vtx : _) -> vUV vtx
              [] -> V2 0 0
         in approxEq u 0.3 && approxEq v 0.7,
    QC.testProperty "projectSpherical on sphere produces [0,1] UVs" $
      once $
        case sphere 1.0 8 6 of
          Just m ->
            let projected = projectSpherical m
             in all
                  ( \vtx ->
                      let V2 u v = vUV vtx
                       in u >= -0.01 && u <= 1.01 && v >= -0.01 && v <= 1.01
                  )
                  (meshVertices projected)
          Nothing -> False,
    QC.testProperty "projectBox preserves vertex count" $
      once $
        case box 1 1 1 1 1 1 of
          Just m ->
            let projected = projectBox 1.0 m
             in meshVertexCount projected == meshVertexCount m
          Nothing -> False
  ]

-- ----------------------------------------------------------------
-- Boolean tests
-- ----------------------------------------------------------------

booleanTests :: [TestTree]
booleanTests =
  [ QC.testProperty "meshUnion produces non-empty result" $
      once $
        case (sphere 1.0 8 6, sphere 1.0 8 6) of
          (Just a, Just b) ->
            let result = meshUnion a (translate (V3 0.5 0 0) b)
             in meshVertexCount result > 0
                  && not (null (meshIndices result))
          _ -> False,
    QC.testProperty "meshIntersection produces valid mesh" $
      once $
        case (sphere 1.0 8 6, sphere 1.0 8 6) of
          (Just a, Just b) ->
            let result = meshIntersection a (translate (V3 0.5 0 0) b)
             in validIndices result && validTriangleCount result
          _ -> False,
    QC.testProperty "meshDifference produces valid mesh" $
      once $
        case (box 2 2 2 1 1 1, sphere 1.0 8 6) of
          (Just a, Just b) ->
            let result = meshDifference a b
             in validIndices result && validTriangleCount result
          _ -> False
  ]

-- ----------------------------------------------------------------
-- Scatter tests
-- ----------------------------------------------------------------

scatterTests :: [TestTree]
scatterTests =
  [ QC.testProperty "scatterUniform produces requested count" $
      once $
        case sphere 1.0 8 6 of
          Just m ->
            let placements = scatterUniform 42 50 m
             in length placements == 50
          Nothing -> False,
    QC.testProperty "triangleArea is positive for non-degenerate" $
      once $
        let area = triangleArea (V3 0 0 0) (V3 1 0 0) (V3 0 1 0)
         in area > 0 && approxEq area 0.5,
    QC.testProperty "scatterPoisson respects minimum distance" $
      once $
        case sphere 1.0 12 8 of
          Just m ->
            let placements = scatterPoisson 42 0.5 200 m
                positions = map plPosition placements
                allFar =
                  and
                    [ vlength (a ^-^ b) >= 0.49
                    | (i, a) <- zip [0 :: Int ..] positions,
                      (j, b) <- zip [0 :: Int ..] positions,
                      i < j
                    ]
             in allFar
          Nothing -> False,
    QC.testProperty "scatterWeighted produces valid placements" $
      once $
        case sphere 1.0 8 6 of
          Just m ->
            let placements = scatterWeighted 42 20 (const 1.0) m
             in length placements == 20
                  && all (\p -> vlength (plNormal p) > 0.5) placements
          Nothing -> False
  ]

-- ----------------------------------------------------------------
-- Symmetry tests
-- ----------------------------------------------------------------

symmetryTests :: [TestTree]
symmetryTests =
  [ QC.testProperty "mirrorX doubles vertex count" $
      once $
        case box 1 1 1 1 1 1 of
          Just m ->
            let mirrored = mirrorX m
             in meshVertexCount mirrored == 2 * meshVertexCount m
          Nothing -> False,
    QC.testProperty "mirrorY doubles vertex count" $
      once $
        case sphere 1.0 6 4 of
          Just m ->
            let mirrored = mirrorY m
             in meshVertexCount mirrored == 2 * meshVertexCount m
          Nothing -> False,
    QC.testProperty "radialSymmetry 4 quadruples vertex count" $
      once $
        case box 1 1 1 1 1 1 of
          Just m ->
            let sym = radialSymmetry (V3 0 1 0) 4 m
             in meshVertexCount sym == 4 * meshVertexCount m
          Nothing -> False,
    QC.testProperty "mirrorX produces valid indices" $
      once $
        case sphere 1.0 8 6 of
          Just m ->
            let mirrored = mirrorX m
             in validIndices mirrored && validTriangleCount mirrored
          Nothing -> False
  ]

-- ----------------------------------------------------------------
-- LOD tests
-- ----------------------------------------------------------------

lodTests :: [TestTree]
lodTests =
  [ QC.testProperty "generateLODChain produces correct level count" $
      once $
        case sphere 1.0 12 8 of
          Just m ->
            let chain = generateLODChain 4 m
             in length chain == 4
          Nothing -> False,
    QC.testProperty "LOD level 0 matches original triangle count" $
      once $
        case sphere 1.0 12 8 of
          Just m ->
            let chain = generateLODChain 3 m
                origTris = length (meshIndices m) `div` 3
             in case chain of
                  (l0 : _) -> lodTriangleCount l0 == origTris
                  [] -> False
          Nothing -> False,
    QC.testProperty "LOD levels have decreasing triangle counts" $
      once $
        case sphere 1.0 16 12 of
          Just m ->
            let chain = generateLODChain 4 m
                triCounts = map lodTriangleCount chain
             in and (zipWith (>=) triCounts (drop 1 triCounts))
          Nothing -> False,
    QC.testProperty "generateLOD with custom ratios" $
      once $
        case sphere 1.0 12 8 of
          Just m ->
            let chain = generateLOD [0.5, 0.25] m
             in length chain == 3
          Nothing -> False
  ]

-- ----------------------------------------------------------------
-- Raycast tests
-- ----------------------------------------------------------------

raycastTests :: [TestTree]
raycastTests =
  [ QC.testProperty "rayTriangle hits centered triangle" $
      once $
        let v0 = V3 (-1) (-1) 2
            v1 = V3 1 (-1) 2
            v2 = V3 0 1 2
            ray = Ray (V3 0 0 0) (V3 0 0 1)
         in isJust (rayTriangle ray (v0, v1, v2)),
    QC.testProperty "rayTriangle misses off-target" $
      once $
        let v0 = V3 (-1) (-1) 2
            v1 = V3 1 (-1) 2
            v2 = V3 0 1 2
            ray = Ray (V3 0 0 0) (V3 0 0 (-1))
         in isNothing (rayTriangle ray (v0, v1, v2)),
    QC.testProperty "rayMesh finds intersection with sphere" $
      once $
        case sphere 1.0 12 8 of
          Just m ->
            let ray = Ray (V3 0 0 (-5)) (normalize (V3 0 0 1))
                hit = rayMesh ray m
             in case hit of
                  Just h ->
                    hitDistance h > 0
                      && hitDistance h < 10
                  Nothing -> False
          Nothing -> False,
    QC.testProperty "BVH gives same result as brute force" $
      once $
        case sphere 1.0 12 8 of
          Just m ->
            let ray = Ray (V3 0 0 (-5)) (normalize (V3 0 0 1))
                bruteHit = rayMesh ray m
                bvh = buildBVH m
                bvhHit = rayBVH ray bvh
             in case (bruteHit, bvhHit) of
                  (Just bh, Just vh) ->
                    approxEq (hitDistance bh) (hitDistance vh)
                  (Nothing, Nothing) -> True
                  _ -> False
          Nothing -> False
  ]

-- ----------------------------------------------------------------
-- Remesh tests
-- ----------------------------------------------------------------

remeshTests :: [TestTree]
remeshTests =
  [ QC.testProperty "remesh produces valid mesh" $
      once $
        case sphere 1.0 6 4 of
          Just m ->
            let result = remesh 0.3 2 m
             in validIndices result
                  && validTriangleCount result
                  && meshVertexCount result > 0
          Nothing -> False,
    QC.testProperty "remesh with zero iterations preserves structure" $
      once $
        case box 1 1 1 1 1 1 of
          Just m ->
            let result = remesh 0.5 0 m
             in meshVertexCount result == meshVertexCount m
          Nothing -> False,
    QC.testProperty "remeshAdaptive produces valid mesh" $
      once $
        case sphere 1.0 8 6 of
          Just m ->
            let result = remeshAdaptive 0.1 0.5 2 m
             in validIndices result && validTriangleCount result
          Nothing -> False
  ]

-- ----------------------------------------------------------------
-- Import tests
-- ----------------------------------------------------------------

importTests :: [TestTree]
importTests =
  [ QC.testProperty "parseOBJ round-trips with meshToOBJ" $
      once $
        case box 1 1 1 1 1 1 of
          Just m ->
            let obj = meshToOBJ m
             in case parseOBJ obj of
                  Just parsed ->
                    meshVertexCount parsed > 0
                      && not (null (meshIndices parsed))
                  Nothing -> False
          Nothing -> False,
    QC.testProperty "parseOBJ handles triangle face format" $
      once $
        let objStr =
              unlines
                [ "v 0 0 0",
                  "v 1 0 0",
                  "v 0 1 0",
                  "vn 0 0 1",
                  "vt 0 0",
                  "vt 1 0",
                  "vt 0 1",
                  "f 1/1/1 2/2/1 3/3/1"
                ]
         in case parseOBJ objStr of
              Just parsed -> meshVertexCount parsed == 3
              Nothing -> False,
    QC.testProperty "parseGLTF round-trips with meshToGLTF" $
      once $
        case box 1 1 1 1 1 1 of
          Just m ->
            let gltf = meshToGLTF m
             in case parseGLTF gltf of
                  Just parsed ->
                    meshVertexCount parsed > 0
                      && not (null (meshIndices parsed))
                  Nothing -> False
          Nothing -> False,
    QC.testProperty "parseOBJ rejects empty string" $
      once $
        isNothing (parseOBJ ""),
    QC.testProperty "parseManyOBJ splits objects" $
      once $
        let objStr =
              unlines
                [ "o first",
                  "v 0 0 0",
                  "v 1 0 0",
                  "v 0 1 0",
                  "f 1 2 3",
                  "o second",
                  "v 0 0 0",
                  "v 1 0 0",
                  "v 0 1 0",
                  "f 1 2 3"
                ]
            result = parseManyOBJ objStr
         in length result == 2
  ]

-- ----------------------------------------------------------------
-- Texture
-- ----------------------------------------------------------------

textureTests :: [TestTree]
textureTests =
  [ QC.testProperty "calcMipLevels 1x1 = 1" $
      once $
        calcMipLevels 1 1 == 1,
    QC.testProperty "calcMipLevels 0 dimension = 1" $
      once $
        calcMipLevels 0 100 == 1
          && calcMipLevels 100 0 == 1,
    QC.testProperty "calcMipLevels power-of-two" $ \(Positive n') ->
      let n = (n' :: Int) `mod` 13 + 1
          s = (1 :: Word32) `shiftL` n
       in calcMipLevels s s == fromIntegral n + 1,
    QC.testProperty "calcMipLevels >= 1" $ \(Positive w) (Positive h) ->
      let w' = min (w :: Word32) 8192
          h' = min h 8192
       in calcMipLevels w' h' >= 1,
    QC.testProperty "calcMipLevels monotonic in width" $
      \(Positive w) (Positive h) ->
        let w' = min (w :: Word32) 4096
            h' = min h 4096
         in calcMipLevels (w' * 2) h' >= calcMipLevels w' h',
    QC.testProperty "calcMipLevels monotonic in height" $
      \(Positive w) (Positive h) ->
        let w' = min (w :: Word32) 4096
            h' = min h 4096
         in calcMipLevels w' (h' * 2) >= calcMipLevels w' h',
    QC.testProperty "calcMipLevels 256x256 = 9" $
      once $
        calcMipLevels 256 256 == 9,
    QC.testProperty "calcMipLevels asymmetric uses max dimension" $
      \(Positive w) (Positive h) ->
        let w' = min (w :: Word32) 4096
            h' = min h 4096
         in calcMipLevels w' h' == calcMipLevels (max w' h') (max w' h')
  ]

-- ----------------------------------------------------------------
-- Scene
-- ----------------------------------------------------------------

sceneTests :: [TestTree]
sceneTests =
  [ QC.testProperty "identityTransform is identity under composition" $
      \(Positive px) (Positive py) (Positive pz) ->
        let t = Transform (V3 px py pz) identityQuat (V3 1 1 1)
         in approxEqTransform (composeTransform identityTransform t) t
              && approxEqTransform (composeTransform t identityTransform) t,
    QC.testProperty "composeTransform is associative" $
      once $
        let a = Transform (V3 1 0 0) identityQuat (V3 1 1 1)
            b = Transform (V3 0 2 0) identityQuat (V3 1 1 1)
            c = Transform (V3 0 0 3) identityQuat (V3 1 1 1)
            ab_c = composeTransform (composeTransform a b) c
            a_bc = composeTransform a (composeTransform b c)
         in approxEqTransform ab_c a_bc,
    QC.testProperty "transformToM44 matches T*R*S" $
      once $
        let t = Transform (V3 1 2 3) (axisAngle (V3 0 1 0) 0.5) (V3 2 2 2)
            m = transformToM44 t
            expected =
              translation (V3 1 2 3)
                `mulM44` rotation (axisAngle (V3 0 1 0) 0.5)
                `mulM44` scaling (V3 2 2 2)
         in approxEqM44 m expected,
    QC.testProperty "emptyScene has one node" $
      once $
        sceneNodeCount emptyScene == 1,
    QC.testProperty "addNode increments node count" $
      once $
        let (_, scene1) = addNode (sceneRoot emptyScene) identityTransform emptyScene
         in sceneNodeCount scene1 == 2,
    QC.testProperty "addNode to invalid parent returns -1" $
      once $
        fst (addNode 999 identityTransform emptyScene) == -1,
    QC.testProperty "removeNode removes descendants" $
      once $
        let (childId, scene1) =
              addNode (sceneRoot emptyScene) identityTransform emptyScene
            (_, scene2) = addNode childId identityTransform scene1
            scene3 = removeNode childId scene2
         in sceneNodeCount scene3 == 1,
    QC.testProperty "removeNode cannot remove root" $
      once $
        let scene' = removeNode (sceneRoot emptyScene) emptyScene
         in sceneNodeCount scene' == 1,
    QC.testProperty "reparent preserves node transform" $
      once $
        let t = Transform (V3 5 0 0) identityQuat (V3 1 1 1)
            (childA, scene1) = addNode 0 t emptyScene
            (childB, scene2) = addNode 0 identityTransform scene1
            scene3 = reparent childA childB scene2
         in getTransform childA scene3 == Just t,
    QC.testProperty "reparent prevents cycles" $
      once $
        let (childId, scene1) = addNode 0 identityTransform emptyScene
            (grandchildId, scene2) = addNode childId identityTransform scene1
            scene3 = reparent childId grandchildId scene2
         in sceneNodeCount scene3 == sceneNodeCount scene2,
    QC.testProperty "worldTransforms root = root local" $
      once $
        let t = Transform (V3 1 2 3) identityQuat (V3 1 1 1)
            scene' = setTransform 0 t emptyScene
            wt = worldTransforms scene'
         in case IntMap.lookup 0 wt of
              Just w -> approxEqTransform w t
              Nothing -> False,
    QC.testProperty "worldTransforms child = parent * child" $
      once $
        let parentT = Transform (V3 10 0 0) identityQuat (V3 1 1 1)
            childT = Transform (V3 0 5 0) identityQuat (V3 1 1 1)
            scene0 = setTransform 0 parentT emptyScene
            (childId, scene1) = addNode 0 childT scene0
            wt = worldTransforms scene1
         in case IntMap.lookup childId wt of
              Just w ->
                approxEqV3 (transformPosition w) (V3 10 5 0)
              Nothing -> False,
    QC.testProperty "worldMatrices matches transformToM44 of worldTransforms" $
      once $
        let parentT = Transform (V3 1 0 0) (axisAngle (V3 0 1 0) 0.3) (V3 2 2 2)
            childT = Transform (V3 0 1 0) identityQuat (V3 1 1 1)
            scene0 = setTransform 0 parentT emptyScene
            (childId, scene1) = addNode 0 childT scene0
            wts = worldTransforms scene1
            wms = worldMatrices scene1
         in case (IntMap.lookup childId wts, IntMap.lookup childId wms) of
              (Just wt, Just wm) -> approxEqM44 (transformToM44 wt) wm
              _ -> False,
    QC.testProperty "worldTransformOf matches worldTransforms" $
      once $
        let parentT = Transform (V3 3 0 0) identityQuat (V3 1 1 1)
            childT = Transform (V3 0 4 0) identityQuat (V3 1 1 1)
            scene0 = setTransform 0 parentT emptyScene
            (childId, scene1) = addNode 0 childT scene0
            wt = worldTransforms scene1
         in case (IntMap.lookup childId wt, worldTransformOf childId scene1) of
              (Just a, Just b) -> approxEqTransform a b
              _ -> False
  ]

-- ----------------------------------------------------------------
-- Camera
-- ----------------------------------------------------------------

cameraTests :: [TestTree]
cameraTests =
  [ QC.testProperty "viewMatrix at origin looking -Z ≈ identity" $
      once $
        let cam = defaultCamera (pi / 4) (16.0 / 9.0)
            vm = viewMatrix cam
         in approxEqM44 vm identity,
    QC.testProperty "lookAtCamera viewMatrix ≈ lookAt" $
      once $
        let eye = V3 0 3 10
            target = V3 0 0 0
            up = V3 0 1 0
            cam = lookAtCamera eye target up (Perspective (pi / 4) 1.0 0.1 100.0)
            vm = viewMatrix cam
            expected = lookAt eye target up
         in approxEqM44 vm expected,
    QC.testProperty "viewProjectionMatrix = projection * view" $
      once $
        let cam =
              lookAtCamera
                (V3 5 5 5)
                (V3 0 0 0)
                (V3 0 1 0)
                (Perspective (pi / 3) (4.0 / 3.0) 0.1 500.0)
            vp = viewProjectionMatrix cam
            expected = projectionMatrix cam `mulM44` viewMatrix cam
         in approxEqM44 vp expected,
    QC.testProperty "defaultCamera produces valid matrices" $
      once $
        let cam = defaultCamera (pi / 4) (16.0 / 9.0)
            M44 (V4 a _ _ _) _ _ _ = viewProjectionMatrix cam
         in not (isNaN a) && not (isInfinite a),
    QC.testProperty "perspective projection matches Math.Matrix" $
      once $
        let cam = Camera vzero identityQuat (Perspective 1.0 1.5 0.1 100.0)
         in approxEqM44 (projectionMatrix cam) (perspective 1.0 1.5 0.1 100.0),
    QC.testProperty "ortho projection matches Math.Matrix" $
      once $
        let cam = Camera vzero identityQuat (Orthographic (-10) 10 (-10) 10 0.1 100.0)
         in approxEqM44 (projectionMatrix cam) (ortho (-10) 10 (-10) 10 0.1 100.0)
  ]

-- ----------------------------------------------------------------
-- Material
-- ----------------------------------------------------------------

materialTests :: [TestTree]
materialTests =
  [ QC.testProperty "packParams produces 16 floats" $
      once $
        length (packParams defaultParams) == 16,
    QC.testProperty "defaultParams roughness > 0" $
      once $
        paramRoughness defaultParams > 0,
    QC.testProperty "defaultParams metallic in [0,1]" $
      once $
        paramMetallic defaultParams >= 0
          && paramMetallic defaultParams <= 1,
    QC.testProperty "packParams albedoFactor matches V4" $
      once $
        let V4 r g b a = paramAlbedoFactor defaultParams
            packed = packParams defaultParams
         in take 4 packed == [r, g, b, a],
    QC.testProperty "defaultMaterial has no textures" $
      once $
        isNothing (materialAlbedo defaultMaterial)
          && isNothing (materialNormal defaultMaterial)
          && isNothing (materialMetallicRoughness defaultMaterial)
          && isNothing (materialAO defaultMaterial)
          && isNothing (materialEmissive defaultMaterial),
    QC.testProperty "defaultParams UV scale is (1,1)" $
      once $
        paramUVScale defaultParams == V2 1 1,
    QC.testProperty "defaultParams UV offset is (0,0)" $
      once $
        paramUVOffset defaultParams == V2 0 0,
    QC.testProperty "packParams emissive at correct offset" $
      once $
        let V4 er eg eb ei = paramEmissiveFactor defaultParams
            packed = packParams defaultParams
         in packed !! 4 == er
              && packed !! 5 == eg
              && packed !! 6 == eb
              && packed !! 7 == ei
  ]

-- ----------------------------------------------------------------
-- Shadow cascade
-- ----------------------------------------------------------------

shadowCascadeTests :: [TestTree]
shadowCascadeTests =
  [ QC.testProperty "splits are monotonically increasing" $
      once $
        let V4 s0 s1 s2 s3 = cascadeSplitDistances 0.1 100.0 0.5
         in s0 < s1 && s1 < s2 && s2 < s3,
    QC.testProperty "splits: first > near, last = far" $
      once $
        let near = 0.1
            far = 100.0
            V4 s0 _ _ s3 = cascadeSplitDistances near far 0.5
         in s0 > near && approxEq s3 far,
    QC.testProperty "lambda=0 gives linear splits" $
      once $
        let near = 1.0
            far = 101.0
            V4 s0 s1 s2 s3 = cascadeSplitDistances near far 0.0
         in approxEq s0 26.0
              && approxEq s1 51.0
              && approxEq s2 76.0
              && approxEq s3 101.0,
    QC.testProperty "lambda=1 gives logarithmic splits" $
      once $
        let near = 1.0
            far = 10000.0
            V4 s0 s1 s2 s3 = cascadeSplitDistances near far 1.0
         in approxEq s0 10.0
              && approxEq s1 100.0
              && approxEq s2 1000.0
              && approxEq s3 10000.0,
    QC.testProperty "frustum corners produces 8 points" $
      once $
        let vp = perspective (pi / 4) 1.0 0.1 100.0 `mulM44` identity
            invVP = inverse vp
            corners = frustumCornersWorld invVP 0.0 1.0
         in length corners == 8,
    QC.testProperty "frustum corners near plane z ≈ -near" $
      once $
        let near = 0.1
            far = 100.0
            proj = perspective (pi / 4) 1.0 near far
            invVP = inverse proj
            corners = frustumCornersWorld invVP 0.0 1.0
            nearCorners = take 4 corners
            allNearZ = all (\(V3 _ _ z) -> approxEq (abs z) near) nearCorners
         in allNearZ,
    QC.testProperty "computeCascades produces 4 valid matrices" $
      once $
        let cfg = defaultShadowConfig
            viewMat = lookAt (V3 0 5 10) (V3 0 0 0) (V3 0 1 0)
            projMat = perspective (pi / 4) (16.0 / 9.0) 0.1 100.0
            lightDir = V3 0.5 1.0 0.3
            cascades = computeCascades cfg viewMat projMat lightDir 0.1 100.0
            M44 (V4 a _ _ _) _ _ _ = cascadeMatrix0 cascades
            M44 (V4 b _ _ _) _ _ _ = cascadeMatrix1 cascades
            M44 (V4 c _ _ _) _ _ _ = cascadeMatrix2 cascades
            M44 (V4 d _ _ _) _ _ _ = cascadeMatrix3 cascades
         in not (isNaN a) && not (isNaN b) && not (isNaN c) && not (isNaN d),
    QC.testProperty "cascade split distances match config" $
      once $
        let cfg = defaultShadowConfig {shadowSplitLambda = 0.5}
            viewMat = lookAt (V3 0 5 10) (V3 0 0 0) (V3 0 1 0)
            projMat = perspective (pi / 4) 1.0 0.1 100.0
            cascades = computeCascades cfg viewMat projMat (V3 0 1 0) 0.1 100.0
            expected = cascadeSplitDistances 0.1 100.0 0.5
         in cascadeSplits cascades == expected,
    QC.testProperty "defaultShadowConfig has resolution 2048" $
      once $
        shadowMapResolution defaultShadowConfig == 2048,
    QC.testProperty "defaultShadowConfig has lambda 0.5" $
      once $
        approxEq (shadowSplitLambda defaultShadowConfig) 0.5
  ]

-- ----------------------------------------------------------------
-- Storable layout assertions
-- ----------------------------------------------------------------

-- | These tests catch GPU struct layout drift. If a Haskell type
-- changes size, the corresponding GLSL layout and C struct must
-- also be updated.
storableLayoutTests :: [TestTree]
storableLayoutTests =
  [ QC.testProperty "Vertex is 64 bytes (matches pipeline vertex stride)" $
      once $
        sizeOf (undefined :: Vertex) == (64 :: Int),
    QC.testProperty "SkinnedVertex is 80 bytes (64 base + 16 bone data)" $
      once $
        sizeOf (undefined :: SkinnedVertex) == (80 :: Int),
    QC.testProperty "MaterialParams is 64 bytes (push constant offset 64-127)" $
      once $
        sizeOf (undefined :: MaterialParams) == (64 :: Int),
    QC.testProperty "FrameUBO is 448 bytes (std140 UBO layout)" $
      once $
        sizeOf (undefined :: FrameUBO) == (448 :: Int)
  ]
