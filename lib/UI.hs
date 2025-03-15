module UI where

import Raylib.Core (beginMode2D, endMode2D, getCharPressed, getKeyPressed)
import Linear (V3(V3), V4 (V4), V2 (V2))
import qualified Data.Text as T
import Raylib.Core.Textures (colorFromNormalized)
import Raylib.Types.Core (Vector4, Color, Rectangle (Rectangle, rectangle'x, rectangle'y, rectangle'height, rectangle'width), KeyboardKey (KeyBackspace, KeyDelete, KeyRight, KeyLeft))
import Raylib.Core.Shapes (drawRectangleV, drawRectangleRec, drawLineV, drawLine)
import Raylib.Core.Text (drawText, measureText)
import Data.Char (chr)
import Raylib.Util.Colors (white)

uiBgCol :: Color
uiBgCol = colorFromNormalized $ V4 0.1 0.1 0.1 0.95

uiFgCol :: Color 
uiFgCol = colorFromNormalized $ V4 0.8 0.8 0.8 0.85

uiPad :: Int 
uiPad = 10

uiLineHeight :: Int
uiLineHeight = 5 

data TextBox = TextBox {
    tbBefore :: T.Text,
    tbAfter :: T.Text,
    tbRect :: Rectangle
}

mkTextbox :: Rectangle -> TextBox
mkTextbox r = TextBox { tbBefore = T.Empty, tbAfter = T.Empty, tbRect = r }

drawTextBox :: TextBox -> IO ()  
drawTextBox tb = do
            drawRectangleRec (tbRect tb) uiBgCol
            drawText bef (x + uiPad) th s uiFgCol
            l <- measureText bef s
            drawLine
                (x + uiPad + l) (th + s)
                (x + uiPad + l + s) (th + s)
                white 
            drawLine
                (x + uiPad + l) (th + s)
                (x + uiPad + l) (th + s - 3)
                white 
            drawText (T.unpack $ tbAfter tb) (x + uiPad + 2 + l) th s uiFgCol
            where
            bef = T.unpack $ tbBefore tb
            Rectangle 
               { rectangle'x = x', 
                 rectangle'y = y',
                 rectangle'height = h',
                 rectangle'width = _ }  = tbRect tb
            x = round x'
            y = round y'
            h = round (0.5 * h')  
            s = round (0.4 * h')
            th = y + h - round (0.2 * h')

textBoxUpdate :: TextBox -> IO TextBox
textBoxUpdate tb = do
        c <- getCharPressed 
        tbNew <- case c of
            c1 | c1 >= 32 && c1 <= 125 -> 
                return $ tb { tbBefore = T.snoc (tbBefore tb) $ chr c1 }
            _ -> return tb
        k <- getKeyPressed 
        case k of
            KeyBackspace -> -- Backspace
                return $ tb { tbBefore = T.dropEnd 1 (tbBefore tb)}
            KeyDelete    -> -- Del
                return $ tb { tbAfter = T.drop 1 (tbAfter tb)}
            KeyRight -> -- Right
                return $ tb { tbAfter = afN, tbBefore = befN}
                where
                af = tbAfter tb
                bef = tbBefore tb
                befN = case T.uncons af of
                        Just (x, _) -> T.snoc bef x
                        _ -> bef
                afN = T.drop 1 af
            KeyLeft -> -- Left 
                return $ tb { tbAfter = afN, tbBefore = befN}
                where
                af = tbAfter tb
                bef = tbBefore tb
                afN = case T.unsnoc bef of
                        Just (_ , x) -> T.cons x af
                        _ -> af 
                befN = T.dropEnd 1 bef
            _ -> return tbNew

textBoxText :: TextBox -> T.Text
textBoxText tb = T.append (tbBefore tb) (tbAfter tb)
        

