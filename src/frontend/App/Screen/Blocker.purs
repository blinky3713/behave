module App.Screen.Blocker where

import Prelude

import App.Screen.Constants as Constants
import App.Screen.Types (class ChangeCoordinates, BoundingBox, Canvas, Cartesian, Point(..), Player(..), _lowerLeft, _upperRight, _x, _y, toCanvas, toCartesian)
import Color.Scheme.MaterialDesign (blueGrey)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens, Lens', lens, (%~), (^.), (.~), (+~), (-~))
import Data.Set (Set, member)
import FRP.Event.Keyboard (Keyboard)
import Graphics.Drawing (Drawing, fillColor, filled, rectangle)

newtype Blocker c =
  Blocker { widthHalf :: Number
          , heightHalf :: Number
          , position :: Point c
          , playerId :: Player
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
  rectangle (blocker ^. _position <<< _x - blocker ^. _widthHalf)
            (blocker ^. _position <<< _y - blocker ^. _heightHalf)
            (blocker ^. _widthHalf * 2.0)
            (blocker ^. _heightHalf * 2.0)

upKey :: forall c. Blocker c -> String
upKey (Blocker b) = case b.playerId of
  Player1 -> Constants.player1Up
  Player2 -> Constants.player2Up

downKey :: forall c. Blocker c -> String
downKey (Blocker b) = case b.playerId of
  Player1 -> Constants.player1Down
  Player2 -> Constants.player2Down

initialBlocker1 :: {w :: Number, h :: Number} -> Blocker Cartesian
initialBlocker1 {w,h} = Blocker { widthHalf: Constants.blockerWidth
                                , heightHalf: Constants.blockerHeight
                                , position: Point { x: 0.0 + Constants.blockerWidth * 2.0
                                                  , y: h / 2.0
                                                  }
                                , playerId: Player1
                                }

initialBlocker2 :: {w :: Number, h :: Number} -> Blocker Cartesian
initialBlocker2 {w,h} = Blocker { widthHalf: Constants.blockerWidth
                                , heightHalf: Constants.blockerHeight
                                , position: Point { x: w - Constants.blockerWidth * 2.0
                                                  , y: h / 2.0
                                                  }
                                , playerId: Player2
                                }


stepBlocker
  :: BoundingBox Cartesian
  -> Set String
  -> Blocker Cartesian
  -> Blocker Cartesian
stepBlocker bb ks =
    adjustForBoundingBox bb <<< stepBlocker'
  where
    stepBlocker' :: Blocker Cartesian -> Blocker Cartesian
    stepBlocker' blocker
      | upKey blocker `member` ks && downKey blocker `member` ks = blocker
      | upKey blocker `member` ks = blocker # _position <<< _y +~ Constants.blockerSpeed
      | downKey blocker `member` ks = blocker # _position <<< _y -~ Constants.blockerSpeed
      | otherwise = blocker

adjustForBoundingBox
  :: BoundingBox Cartesian
  -> Blocker Cartesian
  -> Blocker Cartesian
adjustForBoundingBox bb =
    adjustForBottomWall >>> adjustForTopWall
  where
    adjustForBottomWall blocker =
      let minY = blocker ^. (_position <<< _y) - heightAdjust
          heightAdjust = blocker ^. _heightHalf
      in if minY < bb ^. _lowerLeft <<< _y
         then blocker # _position <<< _y .~ (bb ^. _lowerLeft <<< _y + heightAdjust)
         else blocker

    adjustForTopWall blocker =
      let maxY = blocker ^. (_position <<< _y) + heightAdjust
          heightAdjust = blocker ^. _heightHalf
      in if maxY > bb ^. _upperRight <<< _y
         then blocker # _position <<< _y .~ (bb ^. _upperRight <<< _y - heightAdjust)
         else blocker
