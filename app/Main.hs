{-# LANGUAGE PatternSynonyms #-}

module Main where

import Raylib.Core (clearBackground, disableCursor, isKeyPressed, isKeyDown, enableCursor)
import Raylib.Core.Camera (updateCamera)
import Raylib.Core.Models (drawGrid, drawModel, genMeshCube, loadModel, loadModelFromMesh, drawLine3D, uploadMesh, drawMesh, loadMaterialDefault, loadModelFromMeshManaged, drawModelWires)
import Raylib.Types (Camera3D (Camera3D), CameraMode (CameraModeFirstPerson, CameraModeOrbital, CameraModeFree), CameraProjection (CameraPerspective), pattern Vector3, Camera2D (Camera2D), pattern Vector2, Rectangle (Rectangle), Camera, KeyboardKey (KeyUp, KeyDown, KeyLeftControl, KeyRightControl, KeyM), Mesh (Mesh), Color)
import Raylib.Util (drawing, mode3D, whileWindowOpen_, withWindow, managed, mode2D)
import Raylib.Util.Colors (orange, white, black, gray)
import UI (mkTextbox, drawTextBox, TextBox(..), textBoxText, NumberBox (NumberBox, nbValue, nbInc, nbRect), updateNumberBox, updateTextBox, drawNumberBox)
import qualified Data.Text as T
import LSystem
import Linear (V3(V3), V2(V2))
import Raylib.Util.Camera (cameraMove)
import Raylib.Util.Math (vector3RotateByAxisAngle, matrixIdentity, matrixTranslate)
import LSystem.Util
import Control.Monad.Writer (runWriter, WriterT (runWriterT), execWriterT, MonadWriter (tell))
import Control.Lens (Identity(runIdentity))
import LSystem.LTrees (parseLTree, LTree, evalLTreeW)
import Raylib.Core.Textures (colorAlpha)
import Data.Map (Map)
import qualified Data.Map as M

modelPath :: String
modelPath = "/home/elias/repos/misc/hs_raylib/assets/Model.obj" 

isF :: Char -> Bool
isF 'F' = True
isF _   = False

camUpDown :: Camera3D -> IO Camera3D
camUpDown cam = do
        up <- isKeyDown KeyUp
        down <- isKeyDown KeyDown 
        dir <- return (if up then 0.1 else if down then -0.1 else 0)
        return $ cameraMove cam (Vector3 0 dir 0)

mkMesh :: T.Text -> [(V3 Float, V3 Float)]
mkMesh t = evalLTreeW plantDrawRules (initialState (V3 0 0.01 0)) (parseLTree t)

w :: Color
w = colorAlpha white 0.7

o :: Color
o = colorAlpha orange 0.7

tbStart = [ mkTextbox 10 $ V2 50 20 ]

nbStart = NumberBox {
            nbValue = 0::Int, 
            nbInc = 1, 
            nbRect = Rectangle 1285 20 65 65
          }

data InputMode = LookAround | InteractUI

data AppState = AppState {
      asTextBoxes :: [TextBox]
    , asNumBoxesInt :: [NumberBox Int]
    , asNumBoxesFloat :: [NumberBox Float]
    , asCam3D :: Camera3D
    , asCam2D :: Camera2D
    , asInputMode :: InputMode
}

initialAppState = AppState { 
          asTextBoxes = tbStart 
        , asNumBoxesInt = [nbStart]
        , asNumBoxesFloat = []
        , asCam3D = Camera3D 
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
        , asInputMode = InteractUI
        }

updateInputMode :: InputMode -> IO InputMode
updateInputMode mode = do
        lctrl <- isKeyDown KeyLeftControl
        rctrl <- isKeyDown KeyRightControl
        m <- isKeyPressed KeyM
        if m && (lctrl || rctrl)
            then do
              let newMode = swap mode
              case newMode of
                LookAround -> disableCursor
                InteractUI -> enableCursor 
              return newMode
            else return mode

        where
        swap InteractUI = LookAround
        swap LookAround = InteractUI

times :: Int -> (a -> a) -> (a -> a)
times x f | x < 2 = f
times x f = f . times (x - 1) f

main :: IO ()
main = do
  withWindow
    1400 
    800
    "test"
    60
    ( \window -> do
        whileWindowOpen_
          ( \appstate ->
              let cam3D = asCam3D appstate
                  cam2D = asCam2D appstate
                  textBoxes = asTextBoxes appstate
                  tb1Text = textBoxText $ head textBoxes
                  numBoxesInt = asNumBoxesInt appstate
                  (initText, ruleMap) = parseRules tb1Text
                  inputMode = asInputMode appstate
                  rules = interactiveRules ruleMap 
              in do
              newMode <- updateInputMode inputMode
              drawing
                ( do
                    clearBackground black 
                    mode3D cam3D
                      ( do 
                          let num = nbValue $ head numBoxesInt
                              production = times num (`produce` rules) initText 
                          mapM_ (\(x, y) -> drawLine3D x y orange) (mkMesh production) 
                          --drawMesh m mat matrixIdentity
                          drawGrid 20 0.5
                          
                       )
                    mode2D cam2D 
                       ( do
                           mapM_ drawTextBox textBoxes
                           mapM_ drawNumberBox numBoxesInt
                       )
                )
              case newMode of 
                LookAround -> do
                    cam3Dn <- camUpDown cam3D 
                        >>= (`updateCamera` CameraModeFirstPerson) 
                    return $ appstate { asCam3D = cam3Dn
                                      , asInputMode = newMode }
                InteractUI -> do
                    tbs <- mapM updateTextBox textBoxes
                    nbsI <- mapM updateNumberBox  numBoxesInt
                    return $ appstate { asTextBoxes = tbs
                                      , asNumBoxesInt = nbsI 
                                      , asInputMode = newMode }
              
          )
          initialAppState 
    )


interactiveRules :: Map Char T.Text -> Rules
interactiveRules m c = case M.lookup c m of
                            Just a -> a
                            Nothing -> T.pack [c]

--plantRules :: Rules 
--plantRules 'X' = T.pack "F+[[X]-uX]-iF[-FuX]+X"
--plantRules 'F' = T.pack "FF"
--plantRules a = T.pack [a]

plantDrawRules :: DrawRulesW [(V3 Float, V3 Float)] 
plantDrawRules 'F' = lineStData
plantDrawRules '-' = return . rotateSt X (-25)  
plantDrawRules 'm' = return . rotateSt Y (-25)  
plantDrawRules 'p' = return . rotateSt Y 25  
plantDrawRules '+' = return . rotateSt X 25  -- . rotateSt Z (-0.1)
plantDrawRules '[' = return . pushSt -- . rotateSt Y 5.5
plantDrawRules ']' = return . popSt -- rotateSt Z 8 . popSt 
plantDrawRules _ = return

