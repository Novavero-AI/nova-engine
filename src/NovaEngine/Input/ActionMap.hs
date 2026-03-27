-- | High-level input action system.
--
-- Pure Haskell layer on top of 'NovaEngine.Input' that provides
-- action maps, composite bindings (WASD → V2), interactions
-- (hold, tap), and swappable contexts.
--
-- @
-- let gameplay = emptyActionMap
--       & bindAction "move" (compositeWASD 26 22 4 7)
--       & bindAction "jump" (keyBinding 44)
--       & bindAction "fire" (mouseBinding 0)
--       & bindAction "aim"  (holdKey 29 0.2)
--
--     menu = emptyActionMap
--       & bindAction "confirm" (keyBinding 40)
--       & bindAction "back"    (keyBinding 41)
--
--     state0 = mkInputState gameplay
--     state1 = updateInputState rawInput state0
--     moveVec = actionV2 "move" state1
--     jumped  = actionPressed "jump" state1
-- @
module NovaEngine.Input.ActionMap
  ( -- * Action map
    ActionMap,
    emptyActionMap,
    bindAction,

    -- * Bindings
    Binding (..),
    keyBinding,
    mouseBinding,
    compositeWASD,
    compositeArrows,
    axisBinding,

    -- * Interactions
    holdKey,
    tapKey,
    doubleTapKey,

    -- * Input state
    InputState,
    mkInputState,
    switchActionMap,
    updateInputState,

    -- * Queries
    actionActive,
    actionPressed,
    actionReleased,
    actionValue,
    actionV2,

    -- * Raw input snapshot (passed from IO layer)
    RawInput (..),
    emptyRawInput,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

-- ----------------------------------------------------------------
-- Raw input snapshot
-- ----------------------------------------------------------------

-- | Per-frame raw input state, captured from the C layer and
-- passed into the pure action system.
data RawInput = RawInput
  { riKeysDown :: ![Int],
    riKeysPressed :: ![Int],
    riKeysReleased :: ![Int],
    riMouseDown :: ![Int],
    riMousePressed :: ![Int],
    riMouseDX :: !Float,
    riMouseDY :: !Float,
    riScrollY :: !Float,
    riTime :: !Float
  }
  deriving (Show, Eq)

-- | Empty raw input (nothing pressed).
emptyRawInput :: RawInput
emptyRawInput =
  RawInput [] [] [] [] [] 0 0 0 0

-- ----------------------------------------------------------------
-- Bindings
-- ----------------------------------------------------------------

-- | How an action maps to raw input.
data Binding
  = -- | Single key (scancode). Active while held.
    KeyBinding !Int
  | -- | Mouse button (0=left, 1=right, 2=middle).
    MouseBinding !Int
  | -- | Composite WASD-style: 4 scancodes → V2.
    -- (up, left, down, right)
    CompositeKeys !Int !Int !Int !Int
  | -- | Mouse axis. 0 = X delta, 1 = Y delta, 2 = scroll.
    AxisMouse !Int !Float
  | -- | Hold: key must be held for @duration@ seconds.
    HoldKey !Int !Float
  | -- | Tap: key pressed and released within @maxDuration@.
    TapKey !Int !Float
  | -- | Double tap: two taps within @window@ seconds.
    DoubleTapKey !Int !Float
  deriving (Show, Eq)

-- | Bind to a keyboard scancode.
keyBinding :: Int -> Binding
keyBinding = KeyBinding

-- | Bind to a mouse button.
mouseBinding :: Int -> Binding
mouseBinding = MouseBinding

-- | WASD composite → V2. Takes scancodes for W, A, S, D.
compositeWASD :: Int -> Int -> Int -> Int -> Binding
compositeWASD = CompositeKeys

-- | Arrow key composite → V2.
-- Default scancodes: up=82, left=80, down=81, right=79.
compositeArrows :: Binding
compositeArrows = CompositeKeys 82 80 81 79

-- | Mouse axis binding with sensitivity multiplier.
-- Axis: 0=X, 1=Y, 2=scroll.
axisBinding :: Int -> Float -> Binding
axisBinding = AxisMouse

-- | Hold key for a duration before activating.
holdKey :: Int -> Float -> Binding
holdKey = HoldKey

-- | Tap: press and release within max duration.
tapKey :: Int -> Float -> Binding
tapKey = TapKey

-- | Double tap within a time window.
doubleTapKey :: Int -> Float -> Binding
doubleTapKey = DoubleTapKey

-- ----------------------------------------------------------------
-- Action map
-- ----------------------------------------------------------------

-- | A named set of action-to-binding mappings. Swap between maps
-- to change input contexts (gameplay, menu, vehicle, etc.).
newtype ActionMap = ActionMap (Map String Binding)
  deriving (Show, Eq)

-- | Empty action map with no bindings.
emptyActionMap :: ActionMap
emptyActionMap = ActionMap Map.empty

-- | Add or replace a binding in the action map.
bindAction :: String -> Binding -> ActionMap -> ActionMap
bindAction name binding (ActionMap m) =
  ActionMap (Map.insert name binding m)

-- ----------------------------------------------------------------
-- Action state (per-action runtime)
-- ----------------------------------------------------------------

data ActionState = ActionState
  { asActive :: !Bool,
    asPressed :: !Bool,
    asReleased :: !Bool,
    asValue :: !Float,
    asValueX :: !Float,
    asValueY :: !Float,
    -- Hold/tap tracking
    asHeldSince :: !Float,
    asLastTapTime :: !Float,
    asPrevActive :: !Bool
  }
  deriving (Show, Eq)

defaultActionState :: ActionState
defaultActionState =
  ActionState False False False 0 0 0 0 0 False

-- ----------------------------------------------------------------
-- Input state
-- ----------------------------------------------------------------

-- | Mutable-ish per-frame input state. Threaded through
-- 'updateInputState' each frame.
data InputState = InputState
  { isActionMap :: !ActionMap,
    isActions :: !(Map String ActionState)
  }
  deriving (Show, Eq)

-- | Create an input state from an action map.
mkInputState :: ActionMap -> InputState
mkInputState am = InputState am Map.empty

-- | Switch to a different action map (e.g. gameplay → menu).
-- Clears all action states.
switchActionMap :: ActionMap -> InputState -> InputState
switchActionMap am _ = mkInputState am

-- | Update all action states from raw input.
-- Call once per frame.
updateInputState :: RawInput -> InputState -> InputState
updateInputState raw (InputState am@(ActionMap bindings) prevStates) =
  let newStates = Map.mapWithKey (updateAction raw prevStates) bindings
   in InputState am newStates

updateAction ::
  RawInput -> Map String ActionState -> String -> Binding -> ActionState
updateAction raw prevStates name binding =
  let prev = Map.findWithDefault defaultActionState name prevStates
   in case binding of
        KeyBinding sc ->
          let down = sc `elem` riKeysDown raw
              pressed = sc `elem` riKeysPressed raw
              released = sc `elem` riKeysReleased raw
           in prev
                { asActive = down,
                  asPressed = pressed,
                  asReleased = released,
                  asValue = if down then 1.0 else 0.0,
                  asPrevActive = asActive prev
                }
        MouseBinding btn ->
          let down = btn `elem` riMouseDown raw
              pressed = btn `elem` riMousePressed raw
           in prev
                { asActive = down,
                  asPressed = pressed,
                  asReleased = not down && asPrevActive prev,
                  asValue = if down then 1.0 else 0.0,
                  asPrevActive = asActive prev
                }
        CompositeKeys upSc leftSc downSc rightSc ->
          let u = if upSc `elem` riKeysDown raw then 1.0 else 0.0
              d = if downSc `elem` riKeysDown raw then 1.0 else 0.0
              l = if leftSc `elem` riKeysDown raw then 1.0 else 0.0
              r = if rightSc `elem` riKeysDown raw then 1.0 else 0.0
              vx = r - l
              vy = u - d
              active = vx /= 0 || vy /= 0
           in prev
                { asActive = active,
                  asPressed = active && not (asPrevActive prev),
                  asReleased = not active && asPrevActive prev,
                  asValueX = vx,
                  asValueY = vy,
                  asValue = if active then 1.0 else 0.0,
                  asPrevActive = asActive prev
                }
        AxisMouse axis sensitivity ->
          let val = case axis of
                0 -> riMouseDX raw * sensitivity
                1 -> riMouseDY raw * sensitivity
                _ -> riScrollY raw * sensitivity
           in prev
                { asActive = val /= 0,
                  asValue = val,
                  asValueX = if axis == 0 then val else 0,
                  asValueY = if axis == 1 then val else 0,
                  asPrevActive = asActive prev
                }
        HoldKey sc duration ->
          let down = sc `elem` riKeysDown raw
              pressed = sc `elem` riKeysPressed raw
              heldSince
                | pressed = riTime raw
                | down = asHeldSince prev
                | otherwise = 0
              elapsed = if down then riTime raw - heldSince else 0
              active = elapsed >= duration
           in prev
                { asActive = active,
                  asPressed = active && not (asPrevActive prev),
                  asReleased = not active && asPrevActive prev,
                  asValue = if active then 1.0 else 0.0,
                  asHeldSince = heldSince,
                  asPrevActive = asActive prev
                }
        TapKey sc maxDuration ->
          let released = sc `elem` riKeysReleased raw
              heldSince =
                if sc `elem` riKeysPressed raw
                  then riTime raw
                  else asHeldSince prev
              elapsed = riTime raw - heldSince
              tapped = released && elapsed < maxDuration && elapsed > 0
           in prev
                { asActive = tapped,
                  asPressed = tapped,
                  asReleased = False,
                  asValue = if tapped then 1.0 else 0.0,
                  asHeldSince = heldSince,
                  asPrevActive = asActive prev
                }
        DoubleTapKey sc window ->
          let released = sc `elem` riKeysReleased raw
              lastTap = asLastTapTime prev
              doubleTapped = released && (riTime raw - lastTap) < window && lastTap > 0
              newLastTap = if released then riTime raw else lastTap
           in prev
                { asActive = doubleTapped,
                  asPressed = doubleTapped,
                  asReleased = False,
                  asValue = if doubleTapped then 1.0 else 0.0,
                  asLastTapTime = newLastTap,
                  asPrevActive = asActive prev
                }

-- ----------------------------------------------------------------
-- Queries
-- ----------------------------------------------------------------

-- | Is the action currently active?
actionActive :: String -> InputState -> Bool
actionActive name (InputState _ states) =
  maybe False asActive (Map.lookup name states)

-- | Was the action just pressed this frame?
actionPressed :: String -> InputState -> Bool
actionPressed name (InputState _ states) =
  maybe False asPressed (Map.lookup name states)

-- | Was the action just released this frame?
actionReleased :: String -> InputState -> Bool
actionReleased name (InputState _ states) =
  maybe False asReleased (Map.lookup name states)

-- | Scalar value of the action (0.0 or 1.0 for buttons,
-- continuous for axes).
actionValue :: String -> InputState -> Float
actionValue name (InputState _ states) =
  maybe 0 asValue (Map.lookup name states)

-- | 2D value of the action (for composite or axis bindings).
actionV2 :: String -> InputState -> (Float, Float)
actionV2 name (InputState _ states) =
  case Map.lookup name states of
    Just s -> (asValueX s, asValueY s)
    Nothing -> (0, 0)
