module LSystem where

import Linear (V3)
import qualified Data.Text as T
import Control.Monad.Writer.Lazy

type Rules = Char -> T.Text 

produce :: T.Text -> Rules -> T.Text
produce input rules = T.foldl  (\res ch -> T.append res (rules ch)) (T.pack "") input

data Turtle = Turtle {
      tuPosition :: V3 Float
    , tuDirection :: V3 Float
    , tuDirectionPerp :: V3 Float
} 

type DrawState = [Turtle]

type DrawRulesIO = Char -> DrawState -> IO DrawState 

type DrawRulesW w = Char -> DrawState -> Writer w DrawState

drawIO :: T.Text -> DrawState -> DrawRulesIO -> IO DrawState 
drawIO input initialSt rules =  T.foldlM' (flip rules) initialSt input

drawW :: Monoid w =>  DrawRulesW w -> T.Text -> DrawState -> (DrawState, w)
drawW rules input initialSt =  runWriter $ T.foldlM' (flip rules) initialSt input
