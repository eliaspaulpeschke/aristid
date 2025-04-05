{-# LANGUAGE PatternSynonyms #-}

module Main where

import Raylib.Core (clearBackground, disableCursor, isKeyPressed, isKeyDown, enableCursor)
import Raylib.Core.Camera (updateCamera)
import Raylib.Core.Models (drawGrid,  drawLine3D)
import Raylib.Types (Camera3D (Camera3D), CameraMode (CameraModeFirstPerson), CameraProjection (CameraPerspective), pattern Vector3, Camera2D (Camera2D), pattern Vector2, Rectangle (Rectangle), KeyboardKey (KeyUp, KeyDown, KeyLeftControl, KeyRightControl, KeyM), Color)
import Raylib.Util (drawing, mode3D, whileWindowOpen_, withWindow, mode2D)
import Raylib.Util.Colors (orange, white, black, blue)
import Graphics.UI.Hayout (HayTree (HayNode), hayRectangle, HayConfig (HayConfig, hayMeasureText, hayLog), HayText (HayText, hayTextText, hayTextFontsize), HaySize (HaySize), HayPos (HayPos), HayElemSize (Auto, Fixed), HayColor (HayColor), HayNodeElem (HayRectangle, hayRectPos, hayRectID, hayRectBackgroundColor))
import Raylib.Core.Text (getFontDefault, measureTextEx)
import qualified Data.Text as T
import Linear (V2(V2), V4(V4))
import Control.Monad.Reader (runReader, ReaderT (runReaderT), MonadIO (liftIO))
import Raylib.Core.Shapes (drawRectangleRec, drawRectangleV)
import Raylib.Core.Textures (colorFromNormalized)

data AppState = AppState {
    asCam2D :: Camera2D,
    asCam3D :: Camera3D
}

initialAppState :: AppState
initialAppState = AppState { 
     asCam3D = Camera3D
             (Vector3 2 1 2)
             (Vector3 0 0 0)
             (Vector3 0 1 0)
             50
             CameraPerspective
   , asCam2D = Camera2D
             (Vector2 0 0)
             (Vector2 0 0)
             0
             1
       }


myMeasureText :: HayText -> IO HaySize 
myMeasureText (HayText {hayTextText=txt, hayTextFontsize=size}) = do
    font <- getFontDefault 
    dims <- measureTextEx font (T.unpack txt) size size
    return $ HaySize dims

hayConf :: HayConfig
hayConf = HayConfig {
      hayMeasureText= myMeasureText 
    , hayLog=putStrLn}

hayBlue = HayColor (V4 0.1 0.1 1.0 1.0)
hayGreen = HayColor (V4 0.1 1.0 0.2 1.0)

type MyTreeType = ReaderT HayConfig IO

myHayTree :: IO (HayTree MyTreeType)
myHayTree = runReaderT 
                (hayRectangle 
                    (T.pack "myRect") 
                    (HayPos $ V2 50 50)
                    Auto 
                    hayBlue
                    [ (hayRectangle 
                       (T.pack "myRect") 
                       (HayPos $ V2 50 50)
                       (Fixed (HaySize $ V2 100 100))
                       hayGreen
                       [])
                    , (hayRectangle 
                       (T.pack "myRect") 
                       (HayPos $ V2 160 50)
                       (Fixed (HaySize $ V2 100 100))
                       hayGreen
                       [])])
                hayConf

renderHayTree :: (HayTree MyTreeType) -> IO ()
renderHayTree (HayNode (HayRectangle {hayRectPos=(HayPos pos)
                                    , hayRectID=_
                                    , hayRectBackgroundColor=(HayColor col)})
                       (HaySize size) 
                       children) = do 
    drawRectangleV pos size $ colorFromNormalized col 
    childElems <- runReaderT (sequence children) hayConf
    mapM_ renderHayTree childElems
    

main :: IO ()
main = do
  withWindow
    1400 
    800
    "test"
    60
    (\window -> do
        whileWindowOpen_
          ( \appstate ->
              let cam3D = asCam3D appstate
                  cam2D = asCam2D appstate
              in
              drawing
                ( do
                    clearBackground black 
                    mode3D cam3D
                      ( do 
                          drawGrid 20 5
                          
                       )
                    mode2D cam2D 
                      ( do
                          myTree <- myHayTree
                          renderHayTree myTree
                          pure appstate
                       )
                )
          )
          initialAppState 
    )

