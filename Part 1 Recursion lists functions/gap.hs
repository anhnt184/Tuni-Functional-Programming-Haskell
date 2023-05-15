gap :: (Char, Char) -> Int -> String -> Int
gap _ _ [] = 0
gap (c1, c2) g s@(x:xs)
  | length s < g+2 = 0
  | x == c1 && head (drop (g+1) s) == c2 = 1 + gap (c1, c2) g xs
  | otherwise = gap (c1, c2) g xs