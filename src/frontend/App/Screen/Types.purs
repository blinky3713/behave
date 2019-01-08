module App.Screen.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', lens, (^.), to)
import Math (pow, sqrt)

data Canvas
data Cartesian

class ChangeCoordinates (f :: Type -> Type) where
  toCartesian :: BoundingBox Canvas -> f Canvas -> f Cartesian
  toCanvas :: BoundingBox Cartesian -> f Cartesian -> f Canvas

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

newtype Point c = Point {x :: Number, y :: Number}

derive instance genericPoint :: Generic (Point c) _

instance showPoint :: Show (Point c) where
  show = genericShow

_x :: forall c. Lens' (Point c) Number
_x = lens (\(Point p) -> p.x) (\(Point p) x' -> Point p {x = x'})

_y :: forall c. Lens' (Point c) Number
_y = lens (\(Point p) -> p.y) (\(Point p) y' -> Point p {y = y'})

instance changeCoordinatesPoint :: ChangeCoordinates Point where
  toCanvas bbCart (Point p) = Point p {y = bbCart ^. _upperRight <<< _y - p.y}
  toCartesian bbCanv (Point p) = Point p {y = bbCanv ^. _lowerLeft <<< _y - p.y}

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
