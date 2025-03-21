module LTest where
import LSystem (DrawState)

data TTree = TNode String [TTree] | TLeaf String

instance Show TTree where
    show (TLeaf t) = "Leaf: " ++ show t ++ " "
    show (TNode t xs) = " { Node " ++ show t ++ " [ " ++ concatMap show xs ++ " ] } "  

parseTTree :: String -> Maybe TTree
parseTTree t = case breakBrackStart t of
                    ("", "") -> Nothing  
                    (a, "")  -> Just $ TLeaf a
                    ("", b)  -> Just $ TNode "" $ innerParse b
                    (a, b)   -> Just $ TNode a $ innerParse b 

innerParse :: String -> [TTree] 
innerParse t = case breakBrackStart t of
                ("", "") -> []
                (x, "") -> [TLeaf x]
                ("", x) -> case getBrackPart x of
                    ("", "") -> []
                    (x1, "") -> innerParse x1 
                    (a, b) -> concatMap innerParse [a, b] 
                (a, b) -> [TNode a (innerParse b)]  

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

--unparseTree :: TTree -> String
--unparseTree (TLeaf t) = t
--unparseTree (TNode t xs) = concat (t : map handle xs)
  --          where
    --        handle x = T.snoc (T.cons '[' $ unparseTree x) ']'
    --
--type TDrawRules = DrawState -> Char -> DrawState

--evalTTreeW :: (Monoid w) => DrawRulesW w -> DrawState -> LTree -> w
--evalTTreeW r st (TLeaf t) = let (_, res) = drawW r t st in res
--evalTTreeW r st (TNode t d) = let (newSt, res) = drawW r t st in
                            ( res <> mconcat (parMap rpar (evalLTreeW r newSt) d))
