module UI where

import Raylib.Core (beginMode2D, endMode2D, getCharPressed, getKeyPressed, getMousePosition, isMouseButtonPressed)
import Linear (V3(V3), V4 (V4), V2 (V2), Metric (distance), R1(_x), R2(_y), R3, R4)
import qualified Data.Text as T
import Raylib.Core.Textures (colorFromNormalized)
import Raylib.Types.Core (Vector4, Color, Rectangle (Rectangle, rectangle'x, rectangle'y, rectangle'height, rectangle'width), KeyboardKey (KeyBackspace, KeyDelete, KeyRight, KeyLeft, KeyEnter), MouseButton (MouseButtonLeft))
import Raylib.Core.Shapes (drawRectangleRec, drawLine, drawRectangleV, drawLineV)
import Raylib.Core.Text (drawText, measureText, drawTextEx, getFontDefault, measureTextEx)
import Data.Char (chr)
import Raylib.Util.Colors (white)
import Raylib.Types (Font)
import Control.Lens ((^.), (&), (%~))

uiBgCol :: Color
uiBgCol = colorFromNormalized $ V4 0.1 0.1 0.1 0.95

uiFgCol :: Color 
uiFgCol = colorFromNormalized $ V4 0.8 0.8 0.8 0.85

uiPad :: Int 
uiPad = 10

uiPadF :: Float
uiPadF = fromIntegral uiPad

uiFont :: IO Font
uiFont = getFontDefault 

uiLineHeight :: Int
uiLineHeight = 5 

rectangleCenter :: Rectangle -> V2 Float
rectangleCenter rect = V2 (x + hw) (y + hh)
        where
        x = rectangle'x rect
        y = rectangle'y rect
        hh = 0.5 * rectangle'height rect
        hw = 0.5 * rectangle'width rect

data TextBox = TextBox {
    tbBefore :: T.Text,
    tbAfter :: T.Text,
    tbCorner :: V2 Float,
    tbSize :: Float
}

mkTextbox :: Float -> V2 Float -> TextBox
mkTextbox s c = TextBox { tbBefore = T.empty
                        , tbAfter = T.empty
                        , tbSize = s
                        , tbCorner = c}

drawTextBox :: TextBox -> IO ()  
drawTextBox tb = do
            let corner = tbCorner tb
                size = tbSize tb
                bef = T.unpack $ tbBefore tb
                af = T.unpack $ tbAfter tb
                befLastLine = let l = lines bef in
                              case l of
                                [] -> ""
                                x -> last x
                newLineEnd = case bef of
                                "" -> False
                                x -> last x == '\n'
                text = bef ++ af
                letterW = size * 0.3
            font <- uiFont
            (V2 w h) <- measureTextEx font text size letterW 
            (V2 _ befH) <- measureTextEx font bef size letterW 
            (V2 befW _) <- if not newLineEnd 
                    then measureTextEx font befLastLine size letterW
                    else return 0
            drawRectangleV 
                corner  
                (V2 (w + 2*uiPadF) (h + 2*uiPadF) ) 
                uiBgCol
            drawTextEx 
                font 
                text 
                (corner + V2 uiPadF uiPadF) 
                size 
                letterW
                uiFgCol
            let start = V2 befW befH
                end = V2 (befW + size) befH
            drawLineV 
                (start + corner + V2 uiPadF uiPadF) 
                (end + corner + V2 uiPadF uiPadF)
                white 

updateTextBox :: TextBox -> IO TextBox
updateTextBox tb = do
        c <- getCharPressed 
        tbNew <- case c of
            c1 | c1 >= 32 && c1 <= 125 -> 
                return $ tb { tbBefore = T.snoc (tbBefore tb) $ chr c1 }
            _ -> return tb
        k <- getKeyPressed 
        case k of
            KeyEnter -> return $ tb { tbBefore = T.snoc (tbBefore tb) '\n' }
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

data (Show a, Num a) => NumberBox a = NumberBox {
    nbValue :: a,
    nbInc :: a,
    nbRect :: Rectangle
}

numberBoxFontSize :: (Show a, Num a) => NumberBox a -> Float
numberBoxFontSize nb = let h = rectangle'height (nbRect nb)
                       in
                         h / 4

numberBoxFontSpacing :: (Show a, Num a) => NumberBox a -> Float
numberBoxFontSpacing nb = let w = rectangle'width (nbRect nb)
                       in
                         w / 12

drawTextCentered :: String -> V2 Float -> Float -> Float -> IO ()
drawTextCentered t pos size spacing = do
        font <- uiFont
        (V2 w h) <- measureTextEx font t size spacing 
        let x = (pos ^._x) - (w/2)
            y = (pos ^._y) - (h/2) 
        drawTextEx font t (V2 x y) size spacing uiFgCol


drawChar :: Char -> Float -> V2 Float -> IO ()
drawChar c size pos = drawTextCentered [c] pos size 0

drawPlus:: Float -> V2 Float -> IO ()
drawPlus = drawChar '+'

drawMinus :: Float -> V2 Float -> IO ()
drawMinus = drawChar '-'

drawNumberBox :: (Show a, Num a) => NumberBox a -> IO ()
drawNumberBox nb = do
            drawRectangleRec (nbRect nb) uiBgCol
            drawTextCentered (show $ nbValue nb) center fontsize spacing
            drawPlus fontsize (center & _y %~ (\x -> x - fontsize))
            drawMinus fontsize (center & _y %~ (+ fontsize))
            where
            center = rectangleCenter $ nbRect nb
            fontsize = numberBoxFontSize nb
            spacing = numberBoxFontSpacing nb

data MouseAt = Plus | Minus | None

updateNumberBox :: (Show a, Num a) => NumberBox a -> IO (NumberBox a) 
updateNumberBox nb = do 
            butt <- isMouseButtonPressed MouseButtonLeft
            if not butt 
              then return nb
              else do
                mousePos <- getMousePosition
                case mouseAt mousePos of
                  Plus -> return $ nb {nbValue = nbValue nb + nbInc nb}
                  Minus -> return $ nb {nbValue = nbValue nb - nbInc nb}
                  None -> return nb
            where
            center = rectangleCenter $ nbRect nb
            fontsize = numberBoxFontSize nb
            plusPos = center & _y %~ (\x -> x - fontsize)
            minusPos = center & _y %~ (+ fontsize)
            clickD =  fontsize
            mouseAt :: V2 Float -> MouseAt
            mouseAt mp
                | distance mp plusPos < clickD = Plus
                | distance mp minusPos < clickD = Minus
                | otherwise = None
    


         

