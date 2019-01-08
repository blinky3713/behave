module App.Screen.Animation (animateBall) where

import Prelude

import App.Screen.Ball as Ball
import App.Screen.Types (BoundingBox(..), Point(..), toCanvas, toCartesian)
import Data.DateTime.Instant (unInstant)
import Data.Lens ((^.))
import Data.Maybe (fromMaybe)
import Data.Newtype (over2)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Console as C
import FRP.Behavior (animate, unfold)
import FRP.Event (Event, withLast)
import FRP.Event.AnimationFrame (animationFrame)
import FRP.Event.Time (withTime)
import Graphics.Canvas (CanvasElement, clearRect, getCanvasHeight, getCanvasWidth, getContext2D)
import Graphics.Drawing (render)

--------------------------------------------------------------------------------
-- | Rendering
--------------------------------------------------------------------------------

animateBall
  :: CanvasElement
  -> Effect (Effect Unit)
animateBall canvas = do
    ctx <- getContext2D canvas
    w <- getCanvasWidth canvas
    h <- getCanvasHeight canvas
    let boundingBoxCanvas = BoundingBox { lowerLeft: Point {x: 0.0, y: h}
                                        , upperRight: Point {x: w, y: 0.0}
                                        }
        boundingBoxCartesian = toCartesian boundingBoxCanvas boundingBoxCanvas
        ballAnimation = unfold (Ball.stepBall boundingBoxCartesian) deltaTimes (Ball.initialBall {w,h})
    animate ballAnimation \ball -> do
      _ <- clearRect ctx {x: 0.0, y: 0.0, width: w, height: h}
      let canvasBall = Ball.Ball { position : toCanvas boundingBoxCartesian $ ball ^. Ball._position
                                 , radius : ball ^. Ball._radius
                                 , velocity : ball ^. Ball._velocity
                                 }
      render ctx (Ball.drawBall canvasBall)

  where

    deltaTimes :: Event Milliseconds
    deltaTimes = map (\a -> fromMaybe (Milliseconds 0.0) (over2 Milliseconds sub a.now <$> a.last)) $
                   withLast (unInstant <<< _.time <$> withTime animationFrame)

