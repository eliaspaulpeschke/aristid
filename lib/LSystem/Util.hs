module LSystem.Util where

import Linear (V3(..), V2(..), R1(_x), R2(_y), R3 (_z), (*^))
import LSystem
import Control.Lens
import Raylib.Types (Mesh (Mesh))
import Control.Monad.Writer (WriterT, MonadWriter (tell))
import Util (perp)
import Raylib.Util.Math (vector3RotateByAxisAngle)
import Raylib.Core.Models (drawLine3D)
import Raylib.Util.Colors (white, orange)

data Axis = X | Y | Z 


toRad :: Float -> Float
toRad d = (d / 180) * pi

rotateV3 :: Axis -> Float -> V3 Float -> V3 Float
rotateV3 X a v = vector3RotateByAxisAngle v (V3 1 0 0) a
rotateV3 Y a v = vector3RotateByAxisAngle v (V3 0 1 0) a 
rotateV3 Z a v = vector3RotateByAxisAngle v (V3 0 0 1) a 

rotateSt :: Axis -> Float -> DrawState -> DrawState
rotateSt axis angle st = turtle { tuDirection = dir } : rest
          where
          turtle = head st
          rest = tail st
          oldDir =  tuDirection turtle
          rad = (pi * angle) / 180 
          dir = rotateV3 axis rad oldDir

moveSt :: DrawState -> DrawState
moveSt st = turtle { tuPosition = newPos }  : rest 
          where
          turtle = head st
          rest = tail st
          pos = tuPosition turtle
          dir = tuDirection turtle
          newPos = pos + dir 

lineSt :: (V3 Float -> V3 Float -> IO ()) -> DrawState -> IO DrawState
lineSt line st =  do
            line pos dir 
            return $ turtle { tuPosition = newPos } : rest
            where
            turtle = head st
            rest = tail st
            pos = tuPosition turtle
            dir = tuDirection turtle
            newPos = pos + dir


meshLine :: Float -> V3 Float -> V3 Float -> MonoidMesh
meshLine scale a b = MonoidMesh $ Mesh
    8                             -- vert count
    8                             -- tri count
    [ a1, a2, a3, a4,             -- verts
      b1, b2, b3, b4
     ]
    [ v2z, v2z, v2z, v2z,          -- tex coords. all zero for now
      v2z, v2z, v2z, v2z]
    Nothing                       
    [ n1, n1, n1, n1, n1, n1,             -- normals
      n2, n2, n2, n2, n2, n2,
      n3, n3, n3, n3, n3, n3,
      n4, n4, n4, n4, n4, n4]   
    Nothing
    Nothing
   (Just [ 1, 4, 0,              -- indices
           1, 5, 4,
           2, 5, 1,
           2, 6, 5,
           3, 6, 2,
           3, 7, 6,
           0, 7, 3,
           0, 4, 7]) 
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    0
    0
    Nothing
    where
    v2z = V2 0 0
    s1 = scale *^ perp b
    s2 = vector3RotateByAxisAngle s1 b (pi / 2)
    s3 = vector3RotateByAxisAngle s2 b (pi / 2)
    s4 = vector3RotateByAxisAngle s3 b (pi / 2) 
    a1 = s1 + a
    a2 = s2 + a 
    a3 = s3 + a
    a4 = s4 + a 
    b1 = a1 + b
    b2 = a2 + b 
    b3 = a3 + b
    b4 = a4 + b 
    n1 = vector3RotateByAxisAngle s1 b (pi / 4)
    n2 = vector3RotateByAxisAngle s2 b (pi / 4)
    n3 = vector3RotateByAxisAngle s3 b (pi / 4)
    n4 = vector3RotateByAxisAngle s4 b (pi / 4)


lineStMesh :: (V3 Float -> V3 Float -> MonoidMesh) -> DrawState -> WriterT MonoidMesh Identity DrawState
lineStMesh line st =  do
            tell $ line pos dir 
            return $ turtle { tuPosition = newPos } : rest
            where
            turtle = head st
            rest = tail st
            pos = tuPosition turtle
            dir = tuDirection turtle
            newPos = pos + dir

lineStData :: DrawState -> WriterT [(V3 Float, V3 Float)] Identity DrawState
lineStData st = do
            tell [(pos, newPos)] 
            return $ turtle { tuPosition = newPos } : rest
            where
            turtle = head st
            rest = tail st
            pos = tuPosition turtle
            dir = tuDirection turtle
            newPos = pos + dir


pushSt :: DrawState -> DrawState
pushSt st = new : st 
           where
            last = head st
            new = Turtle { tuPosition = tuPosition last, tuDirection = tuDirection last }

popSt :: DrawState -> DrawState
popSt st = if length st < 2 
             then st 
             else  tail st   

initialState :: V3 Float -> DrawState
initialState dir = [Turtle { 
                       tuPosition = V3 0 0 0, 
                       tuDirection = dir }]

newtype MonoidMesh = MonoidMesh Mesh

meshFromMonoidMesh :: MonoidMesh -> Mesh
meshFromMonoidMesh (MonoidMesh m) = m

(+!+) :: Maybe [a] -> Maybe [a] -> Maybe [a]
Nothing +!+ Nothing = Nothing
(Just a) +!+ Nothing = Just a
Nothing +!+ (Just a) = Just a
(Just a) +!+ (Just b) = Just (a ++ b)

-- TODO: Determine what to do if some maybe value is Nothing in a and something in b
-- Right now, that leads to misaligned data
--
instance Semigroup MonoidMesh where
    a <> b = MonoidMesh $ Mesh 
                (vC1 + vC2) 
                (tC1 + tC2) 
                (verts1 ++ verts2) 
                (texco1 ++ texco2)
                (texco21 +!+ texco22)  
                (norms1 ++ norms2) 
                (tangs1 +!+ tangs2)
                (cols1 +!+ cols2)
                (inds1 +!+ fmap (map (+ fromIntegral vC1)) inds2)
                (animVs1 +!+ animVs2)
                (animNorms1 +!+ animNorms2)
                (boneIds1 +!+ boneIds2)
                (boneWs1 +!+ boneWs2)
                (boneMs1 +!+ boneMs2)
                (boneCount1 + boneCount2) 
                0 
                Nothing
--                       
--                       
            where
            (Mesh vC1 tC1 verts1 texco1 texco21 norms1 tangs1 cols1 inds1 animVs1 animNorms1 boneIds1 boneWs1 boneMs1 boneCount1 _ _) = meshFromMonoidMesh a
            (Mesh vC2 tC2 verts2 texco2 texco22 norms2 tangs2 cols2 inds2 animVs2 animNorms2 boneIds2 boneWs2 boneMs2 boneCount2 _ _) = meshFromMonoidMesh b

instance Monoid MonoidMesh where
    mempty = MonoidMesh $ Mesh 0 0 [] [] Nothing [] Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0 0 Nothing
