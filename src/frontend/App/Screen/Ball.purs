module App.Screen.Ball where

import Prelude

import App.Screen.Blocker as Blocker
import App.Screen.Constants as Constants
import App.Screen.Types (class ChangeCoordinates, BoundingBox, Canvas, Cartesian, Player(..), Point(..), Velocity(..), _lowerLeft, _speed, _upperRight, _vx, _vy, _x, _y, toCanvas, toCartesian)
import Color.Scheme.MaterialDesign (blueGrey)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens, Lens', lens, (%~), (+~), (.~), (^.))
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Graphics.Drawing (Drawing, circle, fillColor, filled)

newtype Ball c =
  Ball { radius :: Number
       , velocity :: Velocity
       , position :: Point c
       }

derive instance genericBall :: Generic (Ball c) _

instance showBall :: Show (Ball c) where
  show = genericShow

instance changeCoordinatesBall :: ChangeCoordinates Ball where
  toCanvas bb ball = ball # _position %~ toCanvas bb
  toCartesian bb ball = ball # _position %~ toCartesian bb


_position :: forall c c'. Lens (Ball c) (Ball c') (Point c) (Point c')
_position = lens (\(Ball b) -> b.position)
                 (\(Ball b) p' -> Ball b {position = p'})

_velocity :: forall c. Lens' (Ball c) Velocity
_velocity = lens (\(Ball b) -> b.velocity)
                 (\(Ball b) v' -> Ball b {velocity = v'})

_radius :: forall c. Lens' (Ball c) Number
_radius = lens (\(Ball b) -> b.radius)
               (\(Ball b) r' -> Ball b {radius = r'})

drawBall :: Ball Canvas -> Drawing
drawBall b = filled (fillColor blueGrey) $ circle (b ^. _position <<< _x) (b ^. _position <<< _y) (b ^. _radius)

initialBall :: {w :: Number, h :: Number} -> Ball Cartesian
initialBall dims = Ball { radius: Constants.ballRadius
                        , velocity: Velocity {direction: {x: 1.0, y: 2.0}, speed: Constants.ballSpeed}
                        , position: Point {x: dims.w / 2.0, y: dims.h / 2.0}
                        }

-- Return either the winner or the updated ball
stepBall
  :: BoundingBox Cartesian
  -> Blocker.Blocker Cartesian
  -> Milliseconds
  -> Ball Cartesian
  -> Either Player (Ball Cartesian)
stepBall bb blocker (Milliseconds dt) ball =
  let s = dt `div` 1000.0
      spd = ball ^. _velocity <<< _speed
      translation = { dx: (ball ^. _velocity <<< _vx) * s * spd
                    , dy: (ball ^. _velocity <<< _vy) * s * spd
                    }
      ball' = ball # _position <<< _x +~ translation.dx
                   # _position <<< _y +~ translation.dy
  in adjustForBlocker blocker <<<  adjustForBoundingBox bb $ ball'

-- TODO: use transformation + symmetry
adjustForBlocker
  :: Blocker.Blocker Cartesian
  -> Ball Cartesian
  -> Either Player (Ball Cartesian)
adjustForBlocker blocker@(Blocker.Blocker b) ball =
    let maxX = ball ^. (_position <<< _x) + radiusB
        minX = ball ^. (_position <<< _x) - radiusB
        radiusB = ball ^. _radius
        centerY = ball ^. (_position <<< _y)
        lowerY = blocker ^. Blocker._position <<< _y - blocker ^. Blocker._heightHalf
        upperY = blocker ^. Blocker._position <<< _y + blocker ^. Blocker._heightHalf
        testBlocker1 = Tuple (minX < (blocker ^. Blocker._position <<< _x)) (centerY <= upperY && centerY >= lowerY)
        testBlocker2 = Tuple (maxX > (blocker ^. Blocker._position <<< _x)) (centerY <= upperY && centerY >= lowerY)
    in case b.playerId of
         Player1 -> case testBlocker1 of
           Tuple false _ -> Right ball
           Tuple true false -> Left Player2
           Tuple true true -> Right (ball # _position <<< _x .~ (blocker ^. Blocker._position ^. _x) + radiusB
                                          # _velocity <<< _vx %~ negate
                                    )
         Player2 -> case testBlocker2 of
           Tuple false _ -> Right ball
           Tuple true false -> Left Player1
           Tuple true true -> Right (ball # _position <<< _x .~ (blocker ^. Blocker._position ^. _x) - radiusB
                                          # _velocity <<< _vx %~ negate
                                    )

adjustForBoundingBox
  :: BoundingBox Cartesian
  -> Ball Cartesian
  -> Ball Cartesian
adjustForBoundingBox bb =
    adjustForBottomWall >>> adjustForTopWall
  where
    adjustForBottomWall ball =
      let minY = ball ^. (_position <<< _y) - radiusB
          radiusB = ball ^. _radius
      in if minY < bb ^. _lowerLeft <<< _y
           then ball # _position <<< _y .~ (bb ^. _lowerLeft <<< _y) + radiusB
                     # (_velocity <<< _vy)  %~ negate
           else ball

    adjustForTopWall ball =
      let maxY = ball ^. (_position <<< _y) + radiusB
          radiusB = ball ^. _radius
      in if maxY > bb ^. _upperRight <<< _y
           then ball # _position <<< _y .~ (bb ^. _upperRight <<< _y) - radiusB
                     # (_velocity <<< _vy) %~ negate
           else ball
