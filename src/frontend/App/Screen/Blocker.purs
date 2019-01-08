module App.Screen.Blocker where

import Prelude

import App.Screen.Types (class ChangeCoordinates, Canvas, Point, _x, _y, toCanvas, toCartesian)
import Color.Scheme.MaterialDesign (blueGrey)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens, Lens', lens, (%~), (^.))
import Graphics.Drawing (Drawing, fillColor, filled, rectangle)

newtype Blocker c =
  Blocker { widthHalf :: Number
          , heightHalf :: Number
          , position :: Point c
          }

derive instance genericBlocker :: Generic (Blocker c) _

instance showBlocker :: Show (Blocker c) where
  show = genericShow

instance changeCoordinatesBlocker :: ChangeCoordinates Blocker where
  toCanvas bb blocker = blocker # _position %~ toCanvas bb
  toCartesian bb blocker = blocker # _position %~ toCartesian bb

_position :: forall c c'. Lens (Blocker c) (Blocker c') (Point c) (Point c')
_position = lens (\(Blocker b) -> b.position)
    (\(Blocker b) p' -> Blocker b {position = p'})

_heightHalf :: forall c. Lens' (Blocker c) Number
_heightHalf = lens (\(Blocker b) -> b.heightHalf)
                   (\(Blocker b) h' -> Blocker b {heightHalf = h'})

_widthHalf :: forall c. Lens' (Blocker c) Number
_widthHalf = lens (\(Blocker b) -> b.widthHalf)
                  (\(Blocker b) w' -> Blocker b {widthHalf = w'})

drawBlocker :: Blocker Canvas -> Drawing
drawBlocker blocker = filled (fillColor blueGrey) $
  rectangle (blocker ^. _position <<< _x) (blocker ^. _position <<< _y)
    (blocker ^. _widthHalf * 2.0) (blocker ^. _heightHalf * 2.0)

--stepBlocker
--  :: BoundingBox Cartesian
--  -> Blocker Cartesian
--  -> Blocker Cartesian
--stepBlocker 
