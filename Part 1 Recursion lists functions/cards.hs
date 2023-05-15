isAceOfSpades :: (Char, Int) -> Bool
isAceOfSpades ('s', 14) = True
isAceOfSpades _ = False

isConsecutiveNumbers :: (Char, Int) -> (Char, Int) -> Bool
isConsecutiveNumbers (s1, n1) (s2, n2) = s1 == s2 && abs (n1 - n2) == 1

isPair :: (Char, Int) -> (Char, Int) -> Bool
isPair (s1, n1) (s2, n2) = n1 == n2

credits :: (Char, Int) -> (Char, Int) -> Int
credits card1 card2
  | isAceOfSpades card1 = 14
  | isAceOfSpades card2 = 14
  | isConsecutiveNumbers card1 card2 = 8
  | isPair card1 card2 = 6
  | abs (snd card1 - snd card2) == 1 = 4
  | fst card1 == fst card2 = 2
  | otherwise = 0