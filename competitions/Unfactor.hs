{-# LANGUAGE FlexibleContexts #-}

-- Exponent divisors
divee y a = divee' y a 1 where
  divee' x a n | x `div` a == 0 = [(0, y)]
  divee' x a n | x `mod` a == 0 = [(n, x `div` a)] ++ divee' (x `div` a) a (n+1)
               | otherwise      = [(0, y)]

-- e.g. solve 24 [4,3,2]
solve 1 xs = return [0 | x <- xs]
solve n [] = []
solve n (a:as) = do
  (k, n') <- divee n a
  ks <- solve n' as
  return (k : ks)

-- Test run with a hard input
main = do
  putStrLn $ show $ solve (2^128 * 3 * 10 * 7 * 4) (reverse [2..20])

-- Makes an input from a list of factors and exponents
mkInput xs ks = foldr (*) 1 (map (uncurry (^)) (zip xs ks))
-- Test the solver
check xs ks = ks `elem` (solve (mkInput xs ks) xs)