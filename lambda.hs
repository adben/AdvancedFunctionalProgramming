-- lambda evaluation using De Bruijn index
type Index = (String, Int)

data Term  = App Term Term
  | Var String
  | Lam String Term
  | Num Int
  deriving (Eq, Show)

-- based on the case
pprint :: Term -> String
pprint (Var n)    = n
pprint (Num x)    = show x
pprint (Lam n e)  = "λ" ++ n ++ "." ++ pprint e
pprint (App e e') = maybeEnclose e ++ " " ++ maybeEnclose e'

maybeEnclose :: Term -> String
maybeEnclose (Var n) = n
maybeEnclose x       = "(" ++ pprint x ++ ")"

pprint' x = putStrLn $ pprint x

findBy :: [Index] -> String -> Int
findBy ((x, y) : xs) x' = if x' == x then y else findBy xs x'
findBy _ _              = error " not found "

bruijn :: Term -> Term
bruijn e = go [] e where
    go m (Lam x e)  = Lam "" $ let m' = map (\(x, y) -> (x, y + 1)) m in go ((x, 0) : m') e
    go m (App e e') = App (go m e) (go m e')
    go m (Num x)    = Num $ show $ findBy m x
    go m (Var n)    = Var $ show $ findBy m n

--TODO finish me


-- (App (Lamb "x" (Var "x")) (Num 3))
-- let exp = (App "x" (lamb "x")(Var 3)) in pprint' exp >> pprint' (bruijn exp)
