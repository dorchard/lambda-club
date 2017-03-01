sierpinskiSquare n h = mapM_ putStrLn $ ss n h '1'
ss 0 h c = replicate h (replicate h c)
ss n h c = (t +++ f +++ t) ++ (f +++ t +++ f) ++ (t +++ f +++ t)
  where
    h' = h `div` 3
    t = ss (n-1) h' c
    f = ss 0 h' '_'
