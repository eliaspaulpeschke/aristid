{-# LANGUAGE PatternSynonyms #-}

module Main where

import Raylib.Core (clearBackground, disableCursor, isKeyPressed, isKeyDown)
import Raylib.Core.Camera (updateCamera)
import Raylib.Core.Models (drawGrid, drawModel, genMeshCube, loadModel, loadModelFromMesh, drawLine3D, uploadMesh, drawMesh, loadMaterialDefault, loadModelFromMeshManaged, drawModelWires)
import Raylib.Types (Camera3D (Camera3D), CameraMode (CameraModeFirstPerson, CameraModeOrbital, CameraModeFree), CameraProjection (CameraPerspective), pattern Vector3, Camera2D (Camera2D), pattern Vector2, Rectangle (Rectangle), Camera, KeyboardKey (KeyUp, KeyDown), Mesh (Mesh), Color)
import Raylib.Util (drawing, mode3D, whileWindowOpen_, withWindow, managed, mode2D)
import Raylib.Util.Colors (orange, white, black, gray)
import UI (mkTextbox, drawTextBox, textBoxUpdate, TextBox(..), textBoxText)
import qualified Data.Text as T
import LSystem
import Linear (V3(V3))
import Raylib.Util.Camera (cameraMove)
import Raylib.Util.Math (vector3RotateByAxisAngle, matrixIdentity, matrixTranslate)
import LSystem.Util
import Control.Monad.Writer (runWriter, WriterT (runWriterT), execWriterT, MonadWriter (tell))
import Control.Lens (Identity(runIdentity))
import LSystem.LTrees (parseLTree, LTree, evalLTreeW)
import Raylib.Core.Textures (colorAlpha)

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


--myMesh :: Mesh
--myMesh = meshFromMonoidMesh $ runIdentity $ execWriterT $ drawW plantData (initialState (V3 0 0.025 0)) plantDrawRules 
{- 
myTree :: LTree
myTree = parseLTree plantData

myMesh :: Mesh 
myMesh = meshFromMonoidMesh $ evalLTreeW plantDrawRules (initialState (V3 0 0.01 0)) myTree
-}

mkMesh :: T.Text -> [(V3 Float, V3 Float)]
mkMesh t = evalLTreeW plantDrawRules (initialState (V3 0 0.01 0)) (parseLTree t)



w :: Color
w = colorAlpha white 0.7

o :: Color
o = colorAlpha orange 0.7

tbStart = mkTextbox (Rectangle 50 50 200 50)

data AppState = AppState {
    asTextBox :: TextBox,
    asCam :: Camera3D,
    asCam2D :: Camera2D
}

initialAppState = AppState { 
        asTextBox = tbStart, 
        asCam = Camera3D 
            (Vector3 2 1 2) 
            (Vector3 0 0 0) 
            (Vector3 0 1 0) 
            50 
            CameraPerspective,
        asCam2D = Camera2D
            (Vector2 0 0)
            (Vector2 0 0)
            0
            1
        }

main :: IO ()
main = do
  withWindow
    1400 
    800
    "test"
    60
    ( \window -> do
        disableCursor

       -- mod <- loadModelFromMeshManaged iom window
--        m <- managed window $ uploadMesh myMesh False
--       mat <- managed window loadMaterialDefault
        --m1 <- managed window $ uploadMesh myMesh1 False

        whileWindowOpen_
          ( \appstate ->
              let c3 = asCam appstate
                  c2 = asCam2D appstate
                  tb = asTextBox appstate
              in do
              drawing
                ( do
                    clearBackground black 
                    mode3D c3
                      ( do
                          mapM_ (\(x, y) -> drawLine3D x y orange) (mkMesh $ textBoxText tb) 
                          --drawMesh m mat matrixIdentity
                          drawGrid 20 0.5
                          
                       )
                    mode2D c2 (drawTextBox tb)
                )
              cam <- camUpDown c3 
              cam2 <- updateCamera cam CameraModeFirstPerson 
              tn <- textBoxUpdate tb
              return $ appstate { asTextBox = tn, asCam = cam2 }
          )
          initialAppState 
    )

plantData = prod $ prod $ prod $ prod $ prod $ prod $ prod $ T.pack "-X" 
            where 
            prod = (`produce` plantRules)

plantRules :: Rules 
plantRules 'X' = T.pack "F+[[X]-uX]-iF[-FuX]+X"
plantRules 'F' = T.pack "FF"
plantRules a = T.pack [a]

plantDrawRules :: DrawRulesW [(V3 Float, V3 Float)] 
plantDrawRules 'F' = lineStData
plantDrawRules '-' = return . rotateSt X (-25)  
plantDrawRules 'i' = return . rotateSt Y (-40)  
plantDrawRules 'u' = return . rotateSt Z (-15)  
plantDrawRules '+' = return . rotateSt X 25  -- . rotateSt Z (-0.1)
plantDrawRules '[' = return . pushSt -- . rotateSt Y 5.5
plantDrawRules ']' = return . popSt -- rotateSt Z 8 . popSt 
plantDrawRules _ = return

