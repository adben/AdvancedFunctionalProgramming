
-- lambda evaluation using De Bruijn index
-- see https://en.wikipedia.org/wiki/De_Bruijn_index
data Term  = App Term Term
  | Lam String Term
  | Var String
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
maybeEnclose (Num n) = show n
maybeEnclose x       = "(" ++ pprint x ++ ")"

pprint' x = putStrLn $ pprint x

findBy :: [(String, Int)] -> String -> Int
findBy ((x, y) : xs) x' = if x' == x then y else findBy xs x'
findBy _ _              = error " not found "

bruijn :: Term -> Term
bruijn e = go [] e where
    go m (Lam x e)  = Lam "" $ let m' = map (\(x, y) -> (x, y + 1)) m in go ((x, 0) : m') e
    go m (App e e') = App (go m e) (go m e')
    go m (Num x)    = Var $ show $ findBy m (show x) --FIXME 
    go m (Var n)    = Var $ show $ findBy m n

-- let expr = (Lam "s" (Lam "a" (Var "a"))) in pprint' expr >> pprint' (bruijn expr)
-- λs.λa.a
-- λ.λ.0

-- let expr = (Lam "s" (Lam "z" (Num 4))) in pprint' expr >> pprint' (bruijn expr)
-- λs.λz.4

-- let expr = (Lam "m" (Lam "n" (Lam "s" (Lam "z" (App (App (Var "m") (Var "s")) (App (App (Var "n") (Var "z")) (Var "s"))))))) in pprint' expr >> pprint' (bruijn expr)
-- λm.λn.λs.λz.(m s) ((n z) s)
-- λ.λ.λ.λ.(3 1) ((2 0) 1)
