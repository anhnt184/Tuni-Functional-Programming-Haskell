onlyDigits :: String -> Bool
onlyDigits "" = False
onlyDigits [x] = x >= '0' && x <= '9'
onlyDigits (x:xs) = x >= '0' && x <= '9' && onlyDigits xs