commonSubstring :: String -> String -> String
commonSubstring [] _ = ""
commonSubstring _ [] = ""
commonSubstring s1@(c1:cs1) s2@(c2:cs2)
    | c1 `elem` s2 = c1 : commonSubstring cs1 (dropWhile (/= c1) s2)
    | c2 `elem` s1 = c2 : commonSubstring (dropWhile (/= c2) s1) cs2
    | otherwise = commonSubstring cs1 cs2
