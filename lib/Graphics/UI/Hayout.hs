{-# LANGUAGE FlexibleContexts#-}

module Graphics.UI.Hayout where

import qualified Data.Text as T
import Linear (V2(..), V4(..))
import Control.Monad.Reader (MonadReader(ask), MonadIO (liftIO))

newtype HayPos = HayPos (V2 Float) 
newtype HaySize = HaySize (V2 Float)
newtype HayID = HayID T.Text
newtype HayColor = HayColor (V4 Float)

data HayText = HayText { hayTextFontsize :: Float 
                       , hayTextText :: T.Text }

data (MonadReader HayConfig m, MonadIO m) => HayTree m = 
      HayNode HayNodeElem HaySize [ m (HayTree m) ]
    | HayLeaf HayLeafElem HaySize

hayTreeElemSize :: (MonadReader HayConfig m, MonadIO m) 
    => HayTree m -> HaySize
hayTreeElemSize (HayNode _ s _) = s
hayTreeElemSize (HayLeaf _ s) = s

data HayElemSize = Auto | Fixed HaySize

-- Can have Children
data HayNodeElem = HayRectangle { hayRectID :: HayID
                                , hayRectPos :: HayPos
                                , hayRectSize :: HayElemSize 
                                , hayRectBackgroundColor :: HayColor
                                , hayRectPadding :: Float
                                , hayRectGap :: Float
                                }
-- Cannot have Children
data HayLeafElem = HayTextElem { hayTextID :: HayID
                               , hayTextElemText :: HayText 
                               }

data HayConfig = HayConfig { 
      hayMeasureText :: HayText -> IO HaySize 
    , hayLog :: String -> IO ()
    }

hayText :: (MonadReader HayConfig m, MonadIO m) 
    => T.Text 
    -> Float 
    -> T.Text 
    -> m (HayTree m)
hayText textID fontSize text = do
        conf <- ask
        let measureText = hayMeasureText conf
            textElem = HayText { hayTextFontsize=fontSize
                               , hayTextText=text
                               }
            leafElem = HayTextElem { hayTextID=HayID textID
                                   , hayTextElemText=textElem
                                   }
        textSize <- liftIO $ measureText textElem
        return $ HayLeaf leafElem textSize

hayRectangle :: (MonadReader HayConfig m, MonadIO m) 
    => T.Text 
    -> HayPos 
    -> HayElemSize
    -> HayColor
    -> Float
    -> Float
    -> [m (HayTree m)]
    -> m (HayTree m)
hayRectangle rectID pos size col pad gap children = do
    childList <- sequence children
    let sizes = map hayTreeElemSize childList
        gaps = (length children - 1) * gap
        width = (2*pad) + gaps +
            foldl (\a (HaySize (V2 b _)) -> a + b) 0 sizes
        height = (2*pad) + foldl (\a (HaySize (V2 _ b)) -> max a b) 0 sizes
        rectElem = HayRectangle { hayRectID=HayID rectID
                                , hayRectPos=pos
                                , hayRectSize=size
                                , hayRectBackgroundColor=col
                                , hayRectPadding=pad
                                , hayRectGap=gap}
    return $ HayNode rectElem (case size of
                                Auto -> HaySize $ V2 width height
                                Fixed x -> x)
                              (processChildren childList (V2 pad pad))
    where
    processChildren :: [HayTree m] -> V2 Float -> [m (HayTree m)]
    processChildren xs pos = foldl 

    
