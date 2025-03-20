module LSystem.LTrees where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as T
import Control.Monad.State.Lazy (StateT(StateT), MonadState (get, put), evalState, evalStateT)
import Control.Monad.Identity (Identity(Identity, runIdentity))
import Data.Foldable1 (Foldable1(toNonEmpty))
import LSystem (DrawRulesW, DrawState, drawW)
import Control.Monad.Writer (execWriterT, WriterT (runWriterT), execWriter, runWriter)
import Control.Parallel.Strategies (parMap, rdeepseq, NFData, rpar)

data LTree =  LNode T.Text [LTree] | LLeaf T.Text

instance Show LTree where
    show (LLeaf t) = "Leaf: " ++ show t ++ " "
    show (LNode t xs) = " { Node " ++ show t ++ " [ " ++ concatMap show xs ++ " ] } "  

parseLTree :: T.Text -> LTree
parseLTree t = case breakBrackStart t of
                    (T.Empty, T.Empty) -> LLeaf t
                    (a, T.Empty)       -> LLeaf a
                    (T.Empty, b)       -> LNode (T.pack "") $ innerParse b
                    (a, b)             -> LNode a $ innerParse b 

innerParse :: T.Text -> [LTree] 
innerParse t = case breakBrackStart t of
                (T.Empty, T.Empty) -> [LLeaf t]
                (x, T.Empty) -> [LLeaf x]
                (T.Empty, x) -> case getBrackPart x of
                    (T.Empty, T.Empty) -> [LLeaf t]
                    (x1, T.Empty) -> innerParse x1 
                    (a, b) -> concatMap innerParse [a, b] 
                (a, b) -> [LNode a (innerParse b)]  

breakBrackStart :: T.Text -> (T.Text, T.Text)
breakBrackStart = T.break (=='[')

getBrackPart :: T.Text -> (T.Text, T.Text)
getBrackPart t = (dropL a, dropR b)  
                where 
                (a, b) = runIdentity $ evalStateT (T.spanM cntBracks t) 0
                dropL = T.drop 1
                dropR = T.drop 1

cntBracks :: Char -> StateT Int Identity Bool
cntBracks c = do
                s <- get
                case c of
                    '[' -> put (s + 1)
                    ']' -> put (s - 1)
                    _   -> pure ()
                s2 <- get
                case s2 of
                    0 -> return False
                    x | x < 0 -> error 
                        "Malformed LSystem String: Brackets do not match"
                    _ -> return True


unparseTree :: LTree -> T.Text
unparseTree (LLeaf t) = t
unparseTree (LNode t xs) = T.concat (t : map handle xs)
            where
            handle x = T.snoc (T.cons '[' $ unparseTree x) ']'

evalLTreeW :: (Monoid w) => DrawRulesW w -> DrawState -> LTree -> w
evalLTreeW r st (LLeaf t) = let (_, res) = drawW r t st in res
evalLTreeW r st (LNode t d) = let (newSt, res) = drawW r t st in
                            ( res <> mconcat (parMap rpar (evalLTreeW r newSt) d))
