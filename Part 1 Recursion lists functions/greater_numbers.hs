nextIsGreater :: [Int] -> [Int]
nextIsGreater xs = [x | (x, y) <- zip xs (tail xs), y > x]