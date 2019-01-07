module App.Screen.Ball where

import Prelude

import App.Screen.Types (Ball(..), BoundingBox(..), Canvas, Cartesian, Point(..), Velocity(..), _lowerLeft, _position, _radius, _speed, _upperRight, _velocity, _vx, _vy, _x, _y)
import Color.Scheme.MaterialDesign (blueGrey)
import Data.Lens ((^.), (%~), (+~), (.~))
import Data.Time.Duration (Milliseconds(..))
import Graphics.Drawing (Drawing, circle, fillColor, filled)


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
