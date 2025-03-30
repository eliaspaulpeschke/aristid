{-# LANGUAGE TypeFamilies, TypeOperators, FlexibleInstances #-}

module LSystem.LTrees where
import LSystem (DrawState, Turtle (tuPosition, tuDirection, tuDirectionPerp, Turtle))
import Control.Parallel.Strategies (parMap, rpar)
import Linear
import Control.Lens 
import Data.MemoTrie
import Control.Arrow (first)
import Raylib.Types (Matrix)

data LTree = LNode String [LTree] | LLeaf String

instance Show LTree where
    show (LLeaf t) = "Leaf: " ++ show t ++ " "
    show (LNode t xs) = " { Node " ++ show t ++ " [ " ++ concatMap show xs ++ " ] } "  

parseLTree :: String -> Maybe LTree
parseLTree t = case breakBrackStart t of
                    ("", "") -> Nothing  
                    (a, "")  -> Just $ LLeaf a
                    ("", b)  -> Just $ LNode "" $ innerParse b
                    (a, b)   -> Just $ LNode a $ innerParse b 

innerParse :: String -> [LTree] 
innerParse t = case breakBrackStart t of
                ("", "") -> []
                (x, "") -> [LLeaf x]
                ("", x) -> case getBrackPart x of
                    ("", "") -> []
                    (x1, "") -> innerParse x1 
                    (a, b) -> concatMap innerParse [a, b] 
                (a, b) -> [LNode a (innerParse b)]  

breakBrackStart :: String -> (String, String)
breakBrackStart = break (=='[')

getBrackPart :: String -> (String, String)
getBrackPart ('[' : xs) = let (_, a, b) = foldl cntBracks (1, "", "") xs in (a, b)
    where
    cntBracks :: (Int, String, String) -> Char ->  (Int, String, String)
    cntBracks (count, bp, rest) '[' = case count of
            0 -> (count, bp, rest ++ ['['])
            _ -> (count + 1, bp ++ ['['], rest)
    cntBracks (count, bp, rest) ']' = case count of
            0 -> (count, bp, rest ++ [']'])
            1 -> (0, bp, rest)
            _ -> (count - 1, bp ++ [']'], rest)
    cntBracks (count, bp, rest)  x  = case count of
            0 -> (count, bp, rest ++ [x])
            _ -> (count, bp ++ [x], rest)

getBrackPart "" = ("", "")
getBrackPart _  = error "getBrackPart got a String that does not start with [" 

type TDrawRules w = Char -> (Matrix, w) -> (Matrix, w)

type DrawFunc w = String -> (Matrix, w)

evalLTreeW :: (Monoid w) => DrawFunc w -> DrawState -> LTree -> w
evalLTreeW f st (LLeaf t) = let (_, res) = f st t in res
evalLTreeW f st (LNode t d) = let (newSt, res) = f st t in
                            ( res <> mconcat (parMap rpar (evalLTreeW f newSt) d))

mkDraw :: (Monoid w) => TDrawRules w -> DrawFunc w
mkDraw rules = memo inner
    where 
    inner state = foldl (flip rules) (state, mempty) 

debugRules :: TDrawRules String
debugRules 'F' (tu, w) = ( [st { tuPosition = newpos }] 
                          , w ++ show (newpos ^._x) ++ "," )
    where
    st = head tu 
    pos = tuPosition  st
    dir = tuDirection st
   -- per = tuDirectionPerp st
    newpos = pos + dir
debugRules _ x = x

enum' :: (HasTrie a) => (a -> a') -> (a :->: b) -> [(a', b)]
enum' f = (fmap.first) f . enumerate

instance HasTrie Float where
  data Float :->: x = FloatTrie ((Integer, Int) :->: x)
  trie f = FloatTrie $ trie (f . uncurry encodeFloat)
  untrie (FloatTrie t) = untrie t . decodeFloat
  enumerate (FloatTrie t) = enum' (uncurry encodeFloat) t

instance (HasTrie a) => HasTrie (V3 a) where
  data (V3 a) :->: x = V3Trie ([a] :->: x)
  trie f = V3Trie $ trie (f . (\x -> V3 (head x) (x !! 1) (x !! 2)))
  untrie (V3Trie t) = untrie t . (\(V3 a b c) -> [a,b,c])
  enumerate (V3Trie t) = enum' (\x -> V3 (head x) (x !! 1) (x !! 2)) t


turtle2List :: Turtle -> [V3 Float]
turtle2List tu = [tuPosition tu, tuDirection tu, tuDirectionPerp tu]

list2Turtle :: [V3 Float] -> Turtle
list2Turtle xs =  Turtle {  tuPosition = head xs 
                             , tuDirection = xs !! 1
                             , tuDirectionPerp = xs !! 2 }  

instance HasTrie Turtle where
  data Turtle :->: x = DrawStateTrie ([V3 Float] :->: x)
  trie f = DrawStateTrie $ trie (f. list2Turtle)
  untrie (DrawStateTrie t) = untrie t . turtle2List 
  enumerate (DrawStateTrie t) = enum' list2Turtle t
