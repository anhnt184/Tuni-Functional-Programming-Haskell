validate :: String -> Bool
validate xs = length xs == 18
           && take 2 xs == "FI"
           && all (\x -> x >= '0' && x <= '9') (drop 2 xs)
           && read (concatMap expand (drop 4 xs ++ take 4 xs)) `mod` 97 == 1
  where
    expand x
      | x >= '0' && x <= '9' = [x]
      | otherwise = show (10 + fromEnum x - fromEnum 'A')