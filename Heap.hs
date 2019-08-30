rev = foldl (flip (:))

-- rev' = foldr (\x r -> r ++ [x]) []

main = print $ rev [1 .. 1000000]

