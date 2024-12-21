module Chelleport.Config where

import Chelleport.Types
import Foreign.C (CFloat)
import qualified SDL

colorWhite :: Color
colorWhite = SDL.V4 255 255 255 255

colorLightGray :: Color
colorLightGray = SDL.V4 100 100 100 255

colorGray :: Color
colorGray = SDL.V4 55 52 65 200

colorAccent :: Color
colorAccent = SDL.V4 110 112 247 255

colorHighlight :: Color
colorHighlight = colorAccent

colorGridLines :: Color
colorGridLines = colorGray

colorFocusLines :: Color
colorFocusLines = colorLightGray

colorAxisLines :: Color
colorAxisLines = colorAccent

colorBackground :: Color
colorBackground = SDL.V4 15 12 25 0

colorFineGrainGrid :: Color
colorFineGrainGrid = SDL.V4 55 52 65 100

windowOpacity :: CFloat
windowOpacity = 0.5

fontSize :: Int
fontSize = 24
