distanceFilter :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
distanceFilter f d s ss = filter (\x -> f x s <= d) ss
