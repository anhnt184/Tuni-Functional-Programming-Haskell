-- Function to calculate distance between two strings based on the count of characters that don't appear in the other string
distance1 :: String -> String -> Float
distance1 s1 s2
  | null s1 && null s2 = 0
  | otherwise = fromIntegral (length (filter (\c -> not (elem c s2)) s1) + length (filter (\c -> not (elem c s1)) s2)) / fromIntegral (length s1 + length s2)

-- Function to calculate distance between two strings based on the count of non-numeric characters in each string
distance2 :: String -> String -> Float
distance2 s1 s2
  | null s1 && null s2 = 0
  | otherwise = fromIntegral (length (filter (\c -> not (elem c ['0'..'9'])) s1) + length (filter (\c -> not (elem c ['0'..'9'])) s2)) / fromIntegral (length s1 + length s2)