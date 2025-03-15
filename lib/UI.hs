module UI where

import Raylib.Core (beginMode2D, endMode2D)
import Linear (V3(V3), V4 (V4), V2 (V2))
import qualified Data.Text as T
import Raylib.Core.Textures (colorFromNormalized)
import Raylib.Types.Core (Vector4, Color, Rectangle (Rectangle, rectangle'x, rectangle'y, rectangle'height, rectangle'width))
import Raylib.Core.Shapes (drawRectangleV, drawRectangleRec)
import Raylib.Core.Text (drawText)

uiBgCol :: Color
uiBgCol = colorFromNormalized $ V4 0.15 0.1 0.1 0.95

uiFgCol :: Color 
uiFgCol = colorFromNormalized $ V4 0.85 0.8 0.8 0.85

uiPad :: Int 
uiPad = 10

uiLineHeight :: Int
uiLineHeight = 5 

uiTextBox :: Rectangle -> T.Text -> IO () 
uiTextBox rect text = do
            drawRectangleRec rect uiBgCol
            drawText (T.unpack text) (x + uiPad) (y + h) s uiFgCol
            where
            Rectangle 
               { rectangle'x = x', 
                 rectangle'y = y',
                 rectangle'height = h',
                 rectangle'width = _ }  = rect
            x = round x'
            y = round y'
            h = round (0.5 * h')
            s = round (0.1 * h')
