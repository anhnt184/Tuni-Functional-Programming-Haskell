-- Function to find all characters that have a number divisible by n
charsDivisibleBy :: Int -> [Char]
charsDivisibleBy n = [c | (c,i) <- zip ['a'..'z'] [1..26], i `mod` n == 0]

-- Function to find all characters that have a number that is a product of any two numbers in ns
charsProductOf :: [Int] -> [Char]
charsProductOf ns = [c | (c,i) <- zip ['a'..'z'] [1..26], any (\x -> any (\y -> x*y == i && x /= y) ns) ns]
