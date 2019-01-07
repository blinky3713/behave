module App.Screen.Animation (animateBall) where

import Prelude

import Color.Scheme.MaterialDesign (blueGrey)
import Data.DateTime.Instant (unInstant)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', lens, to, (%~), (+~), (.~), (^.))
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
import Graphics.Drawing (Drawing, circle, fillColor, filled, render)
import Math (pow, sqrt)

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
        ballAnimation = unfold (stepBall boundingBoxCartesian) deltaTimes (initialBall {w,h})
    animate ballAnimation \ball -> do
      C.log $ show ball
      _ <- clearRect ctx {x: 0.0, y: 0.0, width: w, height: h}
      let canvasBall = Ball { position : toCanvas boundingBoxCartesian $ ball ^. _position
                            , radius : ball ^. _radius
                            , velocity : ball ^. _velocity
                            }
      render ctx (drawBall canvasBall)
  where

    deltaTimes :: Event Milliseconds
    deltaTimes = map (\a -> fromMaybe (Milliseconds 0.0) (over2 Milliseconds sub a.now <$> a.last)) $
                   withLast (unInstant <<< _.time <$> withTime animationFrame)

    drawBall :: Ball Canvas -> Drawing
    drawBall b = filled (fillColor blueGrey) $ circle (b ^. _position <<< _x) (b ^. _position <<< _y) (b ^. _radius)

    initialBall :: {w :: Number, h :: Number} -> Ball Cartesian
    initialBall dims = Ball { radius: 50.0
                            , velocity: Velocity {direction: {x: 1.0, y: 2.0}, speed: 50.0}
                            , position: Point {x: dims.w / 2.0, y: dims.h / 2.0}
                            }

stepBall
  :: BoundingBox Cartesian
  -> Milliseconds
  -> Ball Cartesian
  -> Ball Cartesian
stepBall bb (Milliseconds dt) ball =
  let s = dt `div` 1000.0
      spd = ball ^. _velocity <<< _speed
      translation = { dx: (ball ^. _velocity <<< _vx) * s * spd
                    , dy: (ball ^. _velocity <<< _vy) * s * spd
                    }
      ball' = ball # _position <<< _x +~ translation.dx
                   # _position <<< _y +~ translation.dy
  in adjustForBoundingBox bb ball'

adjustForBoundingBox
  :: BoundingBox Cartesian
  -> Ball Cartesian
  -> Ball Cartesian
adjustForBoundingBox bb =
    adjustForLeftWall >>> adjustForRightWall >>> adjustForBottomWall >>> adjustForTopWall
  where
    adjustForLeftWall ball =
      let minX = ball ^. (_position <<< _x) - radiusB
          radiusB = ball ^. _radius
      in if minX < bb ^. _lowerLeft <<< _x
           then ball # _position <<< _x .~ (bb ^. _lowerLeft <<< _x) + radiusB
                     # (_velocity <<< _vx)  %~ negate
           else ball

    adjustForBottomWall ball =
      let minY = ball ^. (_position <<< _y) - radiusB
          radiusB = ball ^. _radius
      in if minY < bb ^. _lowerLeft <<< _y
           then ball # _position <<< _y .~ (bb ^. _lowerLeft <<< _y) + radiusB
                     # (_velocity <<< _vy)  %~ negate
           else ball

    adjustForRightWall ball =
      let maxX = ball ^. (_position <<< _x) + radiusB
          radiusB = ball ^. _radius
      in if maxX > bb ^. _upperRight <<< _x
           then ball # _position <<< _x .~ (bb ^. _upperRight ^. _x) - radiusB
                     # (_velocity <<< _vx) %~ negate
           else ball

    adjustForTopWall ball =
      let maxY = ball ^. (_position <<< _y) + radiusB
          radiusB = ball ^. _radius
      in if maxY > bb ^. _upperRight <<< _y
           then ball # _position <<< _y .~ (bb ^. _upperRight <<< _y) - radiusB
                     # (_velocity <<< _vy) %~ negate
           else ball

--------------------------------------------------------------------------------
-- | Types and Lenses
--------------------------------------------------------------------------------

newtype Velocity =
  Velocity { direction :: { x :: Number
                          , y :: Number
                          }
           , speed :: Number
           }

derive instance genericVelocity :: Generic Velocity _

instance showVelocity :: Show Velocity where
  show = genericShow

_vx :: Lens' Velocity Number
_vx = lens (\(Velocity v) -> v.direction.x)
          (\(Velocity v) x' -> Velocity v {direction = v.direction {x = x'}})

_vy :: Lens' Velocity Number
_vy = lens (\(Velocity v) -> v.direction.y)
          (\(Velocity v) y' -> Velocity v {direction = v.direction {y = y'}})

_speed :: Lens' Velocity Number
_speed = lens (\(Velocity v) -> v.speed)
              (\(Velocity v) s' -> Velocity v {speed = s'})

data Canvas
data Cartesian

newtype Point c = Point {x :: Number, y :: Number}

derive instance genericPoint :: Generic (Point c) _

instance showPoint :: Show (Point c) where
  show = genericShow

_x :: forall c. Lens' (Point c) Number
_x = lens (\(Point p) -> p.x) (\(Point p) x' -> Point p {x = x'})

_y :: forall c. Lens' (Point c) Number
_y = lens (\(Point p) -> p.y) (\(Point p) y' -> Point p {y = y'})

class ChangeCoordinates (f :: Type -> Type) where
  toCartesian :: BoundingBox Canvas -> f Canvas -> f Cartesian
  toCanvas :: BoundingBox Cartesian -> f Cartesian -> f Canvas

instance changeCoordinatesPoint :: ChangeCoordinates Point where
  toCanvas bbCart (Point p) = Point p {y = bbCart ^. _upperRight <<< _y - p.y}
  toCartesian bbCanv (Point p) = Point p {y = bbCanv ^. _lowerLeft <<< _y - p.y}

newtype Ball c =
  Ball { radius :: Number
       , velocity :: Velocity
       , position :: Point c
       }

derive instance genericBall :: Generic (Ball c) _

instance showBall :: Show (Ball c) where
  show = genericShow

_position :: forall c. Lens' (Ball c) (Point c)
_position = lens (\(Ball b) -> b.position)
                 (\(Ball b) p' -> Ball b {position = p'})

_velocity :: forall c. Lens' (Ball c) Velocity
_velocity = lens (\(Ball b) -> b.velocity)
                 (\(Ball b) v' -> Ball b {velocity = v'})

_radius :: forall c. Lens' (Ball c) Number
_radius = lens (\(Ball b) -> b.radius)
               (\(Ball b) r' -> Ball b {radius = r'})

newtype BoundingBox c =
  BoundingBox { lowerLeft :: Point c
              , upperRight :: Point c
              }

derive instance genericBoundingBox :: Generic (BoundingBox c) _

_lowerLeft :: forall c. Lens' (BoundingBox c) (Point c)
_lowerLeft = lens (\(BoundingBox b) -> b.lowerLeft) (\(BoundingBox b) p -> BoundingBox b {lowerLeft = p})

_upperRight :: forall c. Lens' (BoundingBox c) (Point c)
_upperRight = lens (\(BoundingBox b) -> b.upperRight) (\(BoundingBox b) p -> BoundingBox b {upperRight = p})


instance changeCoordinatesBoundingBox :: ChangeCoordinates BoundingBox where
  toCartesian bb (BoundingBox bb') = BoundingBox bb' { lowerLeft = bb ^. _lowerLeft <<< to (toCartesian bb)
                                                     , upperRight = bb ^. _upperRight <<< to (toCartesian bb)
                                                     }

  toCanvas bb (BoundingBox bb') = BoundingBox bb' { lowerLeft = bb ^. _lowerLeft <<< to (toCanvas bb)
                                                  , upperRight = bb ^. _upperRight <<< to (toCanvas bb)
                                                  }

--------------------------------------------------------------------------------
-- | Utils
--------------------------------------------------------------------------------

distance1 :: Number -> Number -> Number
distance1 x1 x2 = sqrt $ (x1 - x2) `pow` 2.0

distance2 :: forall c. Point c -> Point c -> Number
distance2 p1 p2 = sqrt $ (p1 ^. _x - p2 ^. _x) `pow` 2.0 + (p1 ^. _y - p2 ^. _y) `pow` 2.0
