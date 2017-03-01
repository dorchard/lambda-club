import System.Environment
sierpinski :: Int -> Int -> IO ()
sierpinski n h = mapM_ putStrLn $ sierpinski' n h

sierpinski' n h = edge h +++ sp n h '1' +++ edge h
   where edge h = [replicate n '_' | n <- [h-1,h-2..0]]

sp :: Int -> Int -> Char -> [String]
sp 0 h c = [replicate (r*2 + 1) c | r <- [0..(h-1)]]
sp n h c = t ++ (t +++ f +++ t)
  where
    t = sp (n-1) h' c
    f = reverse (sp 0 h' '_')
    h' = h `div` 2

(+++) = zipWith (++)

main = do
  [n] <- getArgs
  let m = read n
  sierpinski m (2 ^ m)