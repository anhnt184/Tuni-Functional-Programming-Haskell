headOrLast :: [String] -> Char -> [String]
headOrLast xs c = [s | s <- xs, head s == c || last s == c]