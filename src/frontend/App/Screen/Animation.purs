module App.Screen.Animation (animateBall) where

import Prelude

import App.Screen.Ball as Ball
import App.Screen.Blocker as Blocker
import App.Screen.Types (BoundingBox(..), Cartesian, Point(..), toCanvas, toCartesian)
import Data.DateTime.Instant (unInstant)
import Data.Foldable (fold)
import Data.Lens ((^.))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (over2)
import Data.Set (Set)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Console as C
import FRP.Behavior (animate, sampleBy, unfold)
import FRP.Behavior.Keyboard (keys)
import FRP.Event (Event, withLast)
import FRP.Event.AnimationFrame (animationFrame)
import FRP.Event.Keyboard (Keyboard, getKeyboard)
import FRP.Event.Time (withTime)
import Graphics.Canvas (CanvasElement, clearRect, getCanvasHeight, getCanvasWidth, getContext2D)
import Graphics.Drawing (render)

--------------------------------------------------------------------------------
-- | Rendering
--------------------------------------------------------------------------------

type GameState =
  { ball :: Maybe (Ball.Ball Cartesian)
  , blocker :: Blocker.Blocker Cartesian
  }

initialGameState :: {w :: Number, h :: Number} -> GameState
initialGameState dims = { ball: Just $ Ball.initialBall dims
                        , blocker: Blocker.initialBlocker dims
                        }

animateBall
  :: CanvasElement
  -> Effect (Effect Unit)
animateBall canvas = do
    ctx <- getContext2D canvas
    w <- getCanvasWidth canvas
    h <- getCanvasHeight canvas
    kb <- getKeyboard
    let boundingBoxCanvas = BoundingBox { lowerLeft: Point {x: 0.0, y: h}
                                        , upperRight: Point {x: w, y: 0.0}
                                        }
        boundingBoxCartesian = toCartesian boundingBoxCanvas boundingBoxCanvas
        gameAnimation = unfold ($) (gameStates boundingBoxCartesian kb) (initialGameState {w,h})
    animate gameAnimation \gs -> do
      _ <- clearRect ctx {x: 0.0, y: 0.0, width: w, height: h}
      let canvasBall = maybe mempty (Ball.drawBall <<< toCanvas boundingBoxCartesian) gs.ball
          canvasBlocker = Blocker.drawBlocker $ toCanvas boundingBoxCartesian gs.blocker
          drawing = fold [canvasBall, canvasBlocker]
      render ctx drawing



stepGameState
  :: BoundingBox Cartesian
  -> Set String
  -- ^ keysPressed
  -> Milliseconds
  -- ^ deltaTime
  -> GameState
  -> GameState
stepGameState bb ks dt gs =
  let nextBlocker = Blocker.stepBlocker bb ks gs.blocker
  in { ball: Ball.stepBall bb nextBlocker dt =<< gs.ball
     , blocker: nextBlocker
     }

gameStates
  :: BoundingBox Cartesian
  -> Keyboard
  -> Event (GameState -> GameState)
gameStates bb kb = sampleBy (stepGameState bb) (keys kb) deltaTimes

-- | deltaTimes is an event that reports the difference between the current animationFrame
-- | and the previous animationFrame.
deltaTimes :: Event Milliseconds
deltaTimes =
  map (\a -> fromMaybe (Milliseconds 0.0) (over2 Milliseconds sub a.now <$> a.last)) $
    withLast (unInstant <<< _.time <$> withTime animationFrame)

