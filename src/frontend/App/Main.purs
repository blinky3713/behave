module App.Main where

import Prelude

import App.Button.Component as B
import App.Screen.Animation (animateBall)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console as C
import Graphics.Canvas (CanvasElement)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Partial.Unsafe (unsafeCrashWith)
import Web.DOM.ParentNode (QuerySelector(..))

foreign import createCanvas :: Effect CanvasElement


main :: Effect Unit
main = C.log "hello" *> do --HA.runHalogenAff do
  canvas <- createCanvas
  void $ animateBall canvas
   -- mappElem <- HA.selectElement $ QuerySelector "#app"
   -- case mappElem of
   --   Nothing -> unsafeCrashWith "div#app has to be defined"
   --   Just appElem -> do
   --     runUI B.component true appElem
