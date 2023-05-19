-- CountryCode type
newtype CountryCode = MakeCountryCode Integer deriving (Eq, Ord)

instance Show CountryCode where
  show (MakeCountryCode code) = '+' : show code

toCountryCode :: Integer -> CountryCode
toCountryCode n
  | n < 0     = error "Negative country code"
  | otherwise = MakeCountryCode n

-- PhoneNo type
newtype PhoneNo = MakePhoneNo Integer deriving (Eq, Ord)

instance Show PhoneNo where
  show (MakePhoneNo no) = show no

toPhoneNo :: Integer -> PhoneNo
toPhoneNo n
  | n < 0     = error "Negative phone number"
  | otherwise = MakePhoneNo n

fromPhoneNo :: PhoneNo -> Integer
fromPhoneNo (MakePhoneNo no) = no

-- PhoneType type
data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Eq, Ord, Read, Show)

-- Phone type
data Phone = Phone
  { phoneType :: Maybe PhoneType
  , countryCode :: Maybe CountryCode
  , phoneNo :: PhoneNo
  } deriving (Eq, Ord)

instance Show Phone where
  show (Phone pType cCode pNo) =
    let typeStr = case pType of
                    Just p -> show p
                    Nothing -> ""
        codeStr = case cCode of
                    Just code -> show code ++ " "
                    Nothing -> ""
        typeSuffix = if null typeStr then "" else " (" ++ typeStr ++ ")"
    in codeStr ++ show pNo ++ typeSuffix

makePhone :: Maybe PhoneType -> Maybe CountryCode -> PhoneNo -> Phone
makePhone pType cCode pNo = Phone pType cCode pNo

-- Helper function: readPhoneType
readPhoneType :: String -> Maybe PhoneType
readPhoneType "WorkLandline" = Just WorkLandline
readPhoneType "PrivateMobile" = Just PrivateMobile
readPhoneType "WorkMobile" = Just WorkMobile
readPhoneType "Other" = Just Other
readPhoneType _ = Nothing

-- Helper function: readCountryCode
readCountryCode :: String -> Maybe CountryCode
readCountryCode str
  | '+' `elem` str = Just (toCountryCode (readInteger (filter (\c -> c >= '0' && c <= '9') str)))
  | "00" `isPrefixOf'` str = Just (toCountryCode (readInteger (drop 2 str)))
  | elem (readInteger str) predefinedCountryCodes = Just (toCountryCode (readInteger str))
  | otherwise = Nothing

-- Helper function: readPhoneNo
readPhoneNo :: String -> PhoneNo
readPhoneNo str
  | not (null str) && all (\c -> c >= '0' && c <= '9') str = toPhoneNo (readInteger str)
  | otherwise = error $ if null str then "Empty phone number" else "Incorrect phone number"

-- Helper function: readInteger
readInteger :: String -> Integer
readInteger = foldl (\acc c -> acc * 10 + charToInt c) 0

-- Helper function: charToInt
charToInt :: Char -> Integer
charToInt '0' = 0
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9
charToInt '-' = error "Negative country code"
charToInt _   = error "Invalid character"


-- Helper function: isPrefixOf' to replace isPrefixOf
isPrefixOf' :: String -> String -> Bool
isPrefixOf' [] _ = True
isPrefixOf' _ [] = False
isPrefixOf' (x:xs) (y:ys)
  | x == y    = isPrefixOf' xs ys
  | otherwise = False

-- readPhone function
readPhone :: String -> String -> String -> Phone
readPhone pTypeStr cCodeStr pNoStr =
  let pType = if null pTypeStr then Nothing else readPhoneType pTypeStr
      cCode = if null cCodeStr then Nothing else readCountryCode cCodeStr
      pNo = readPhoneNo pNoStr
  in makePhone pType cCode pNo
