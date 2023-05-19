import Text.Read (readMaybe)

calculate :: Int -> Char -> Int -> Maybe Int
calculate x '+' y = Just (x + y)
calculate x '-' y = Just (x - y)
calculate x '*' y = Just (x * y)
calculate _ _ _    = Nothing

processLine :: String -> String
processLine line = case words line of
  [x, op, y] -> case (readMaybe x, readMaybe y) of
    (Just num1, Just num2) -> case calculate num1 (head op) num2 of
      Just result -> show result
      Nothing     -> "I cannot calculate that"
    _ -> "I cannot calculate that"
  _ -> "I cannot calculate that"

main :: IO ()
main = do
  line <- getLine
  if line == "quit"
    then putStrLn "bye"
    else do
      putStrLn (processLine line)
      main
