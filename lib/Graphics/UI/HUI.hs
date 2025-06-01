{-# LANGUAGE GADTs #-}
module Graphics.UI.HUI where

import qualified Data.Text as T
import Linear (V2 (..), V3 (..),V4 (..))

data RenderLineConfig = LineConfig {
    lineThickness :: Float,
    lineColor :: V4 Float }

data RenderCmd = Line {
        lineStart :: V2 Float,
        lineEnd :: V2 Float,
        lineConfig :: RenderLineConfig}
    | Rect {
        rectCorner :: V2 Float, -- Upper left corner
        rectSize :: V2 Float,
        rectLineConfig :: RenderLineConfig,
        rectFill :: V2 Float }
    | Circle {
        circleCenter :: V2 Float,
        circleRadius :: Float,
        circleLineConfig :: RenderLineConfig,
        circleFill :: V2 Float }
    | Text {
        textPosition :: V2 Float,
        textSize :: V2 Float,
        textFont :: String,
        textText :: String,
        textLineConfig :: RenderLineConfig} 
        -- TODO: this is unsafe, find a way to 
        -- handle Fonts and Text size sensibly

data RenderError = RenderError String

class ElementData a where
    size :: V2 Float -> a -> (V2 Float, a) -- Input: Minimum size
    getSize :: a -> V2 Float
    canGrow :: a -> Bool
    grow :: a -> V2 Float -> a
    grow' :: (a -> V2 Float -> a) -> a -> V2 Float -> a --Custom grow algo 
    positionChildren :: (a, [HUITree]) -> Either RenderError HUITree
    --setPosition :: a -> V2 Float -> a
    --getPosition :: a -> Maybe (V2 Float)
    render :: a -> Either RenderError [RenderCmd]

data HUITree where
    Node :: ElementData a => a -> [HUITree] -> HUITree
    Leaf :: ElementData a => a -> HUITree

sizeTree :: HUITree -> HUITree
sizeTree node = snd $ innerSizeTree node 
    where 
    innerSizeTree :: HUITree -> (V2 Float, HUITree)
    innerSizeTree (Node el rest) = (s, Node e children) 
        where
        (s, e) = size childrenSize el
        (childrenSize, children) = foldl 
            (\(cs, cl) c -> let (x, y) = innerSizeTree c
                            in (cs + x, cl ++ [y]))
            (V2 0 0, [])
            rest
    innerSizeTree (Leaf el) = let (s, e) = size (V2 0 0) el
                            in (s, Leaf e)

positionTree :: HUITree -> Either RenderError HUITree
positionTree (Node el rest) = positionChildren (el, rest)
positionTree (Leaf el) = Right (Leaf el)

renderTree :: HUITree -> Either RenderError [RenderCmd] 
renderTree (Node el rest) = case render el of
    Left err -> Left err
    Right res -> foldl (\inp out -> case out of
            (Left err) -> Left err
            (Right list) -> case inp of
                (Left err) -> Left err
                (Right list2) -> Right (list ++ list2))
        (Right res)
        (map renderTree rest) 
renderTree (Leaf el) = render el 
