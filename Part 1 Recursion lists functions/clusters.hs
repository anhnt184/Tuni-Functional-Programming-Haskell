clusters :: (String -> String -> Float) -> Float -> [String] -> [[String]]
clusters f d ss = go ss []
  where
    go [] acc = acc
    go (x:xs) acc = go xs (cluster f d x ss : acc)

cluster :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
cluster f d s ss = filter (\x -> f x s <= d) ss
