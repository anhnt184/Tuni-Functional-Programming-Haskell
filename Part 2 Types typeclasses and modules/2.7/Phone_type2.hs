module Phone_type2
  ( Phone(..)
  , CountryCode(..)
  , PhoneNo(..)
  , PhoneType(..)
  , predefinedCountryCodes
  , readPhone
  , readCountryCode
  , readPhoneType
  , readPhoneNo
  ) where

newtype CountryCode = CountryCode Integer deriving (Eq, Ord)

instance Show CountryCode where
  show (CountryCode code) = '+' : show code

instance Num CountryCode where
  fromInteger n
    | n < 0     = error "Negative country code"
    | otherwise = CountryCode n
  (+) (CountryCode a) (CountryCode b) = fromInteger $ a + b
  (-) (CountryCode a) (CountryCode b) = fromInteger $ a - b
  (*) (CountryCode a) (CountryCode b) = fromInteger $ a * b

toCountryCode :: Integer -> CountryCode
toCountryCode n
  | n < 0     = error "Negative country code"
  | otherwise = CountryCode n

newtype PhoneNo = PhoneNo Integer deriving (Eq, Ord)

instance Show PhoneNo where
  show (PhoneNo no) = show no

instance Num PhoneNo where
  fromInteger n
    | n < 0     = error "Negative phone number"
    | otherwise = PhoneNo n
  (+) (PhoneNo a) (PhoneNo b) = fromInteger $ a + b
  (-) (PhoneNo a) (PhoneNo b) = fromInteger $ a - b
  (*) (PhoneNo a) (PhoneNo b) = fromInteger $ a * b

toPhoneNo :: Integer -> PhoneNo
toPhoneNo n
  | n < 0     = error "Negative phone number"
  | otherwise = PhoneNo n

data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Eq, Ord, Read, Show)

data Phone = Phone
  { phoneType :: PhoneType
  , countryCode :: CountryCode
  , phoneNo :: PhoneNo
  } deriving (Eq, Ord)

instance Show Phone where
  show (Phone pType cCode pNo) = show cCode ++ " " ++ show pNo



predefinedCountryCodes :: [Integer]
predefinedCountryCodes = [44, 358]

readPhone :: String -> String -> String -> Phone
readPhone pTypeStr cCodeStr pNoStr = Phone pType cCode pNo
  where
    pType = readPhoneType pTypeStr
    cCode = readCountryCode cCodeStr
    pNo = readPhoneNo pNoStr

readCountryCode :: String -> CountryCode
readCountryCode str
  | '+' `elem` str = toCountryCode (readInteger (filter (\c -> c >= '0' && c <= '9') str))
  | "00" `isPrefixOf'` str = toCountryCode (readInteger (drop 2 str))
  | elem (readInteger str) predefinedCountryCodes = toCountryCode (readInteger str)
  | otherwise = error $ if null str then "Empty country code" else "Unknown country code"

readPhoneType :: String -> PhoneType
readPhoneType "WorkLandline" = WorkLandline
readPhoneType "PrivateMobile" = PrivateMobile
readPhoneType "WorkMobile" = WorkMobile
readPhoneType "Other" = Other
readPhoneType _ = error "Incorrect phone type"

readPhoneNo :: String -> PhoneNo
readPhoneNo str
  | not (null str) && all (\c -> c >= '0' && c <= '9') str = toPhoneNo (readInteger str)
  | otherwise = error $ if null str then "Empty phone number" else "Incorrect phone number"

isPrefixOf' :: String -> String -> Bool
isPrefixOf' [] _ = True
isPrefixOf' _ [] = False
isPrefixOf' (x:xs) (y:ys)
  | x == y    = isPrefixOf' xs ys
  | otherwise = False

readInteger :: String -> Integer
readInteger = foldl (\acc c -> acc * 10 + charToInt c) 0

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
charToInt _   = error "Unknown country code"
