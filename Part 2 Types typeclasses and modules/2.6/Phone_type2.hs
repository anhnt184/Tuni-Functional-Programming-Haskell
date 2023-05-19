-- Phone_type2 module
module Phone_type2
  ( Phone(..)
  , CountryCode(..)
  , PhoneBookEntry(..)
  , readPhone
  , fromPhoneNo
  , toPhoneNo
  ) where

-- Import necessary modules
import Data.List (isPrefixOf)

-- PhoneBookEntry type
data PhoneBookEntry = PhoneBookEntry
  { entryName :: String
  , entryPhone :: Phone
  } deriving (Eq, Ord)

instance Show PhoneBookEntry where
  show (PhoneBookEntry name phone) = name ++ ": " ++ show phone

-- CountryCode type
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
  abs (CountryCode a) = CountryCode (abs a)
  signum (CountryCode a) = CountryCode (signum a)

-- PhoneNo type
newtype PhoneNo = PhoneNo Integer deriving (Eq, Ord)

instance Show PhoneNo where
  show (PhoneNo n) = show n

instance Num PhoneNo where
  fromInteger n
    | n < 0     = error "Negative phone number"
    | otherwise = PhoneNo n
  (+) (PhoneNo a) (PhoneNo b) = fromInteger $ a + b
  (-) (PhoneNo a) (PhoneNo b) = fromInteger $ a - b
  (*) (PhoneNo a) (PhoneNo b) = fromInteger $ a * b
  abs (PhoneNo a) = PhoneNo (abs a)
  signum (PhoneNo a) = PhoneNo (signum a)

-- PhoneType type
data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Eq, Ord, Read, Show)

-- readPhone function
readPhone :: String -> String -> String -> Phone
readPhone pTypeStr cCodeStr pNoStr = Phone pType cCode pNo
  where
    pType = readPhoneType pTypeStr
    cCode = readCountryCode cCodeStr
    pNo = readPhoneNo pNoStr

-- Helper function: isPrefixOf' to replace isPrefixOf
isPrefixOf' :: String -> String -> Bool
isPrefixOf' [] _ = True
isPrefixOf' _ [] = False
isPrefixOf' (x:xs) (y:ys)
  | x == y    = isPrefixOf' xs ys
  | otherwise = False

-- Helper function: readInteger to replace read
readInteger :: String -> Integer
readInteger = foldl (\acc c -> acc * 10 + charToInt c) 0

-- Helper function: charToInt to convert a character to an integer
charToInt :: Char -> Integer
charToInt c
  | c `elem` ['0'..'9'] = toInteger $ fromEnum c - fromEnum '0'
  | otherwise           = error $ "Invalid digit: " ++ [c]

-- Helper function: readPhoneType to read PhoneType from string
readPhoneType :: String -> PhoneType
readPhoneType str
  | isPrefixOf' "WorkLandline" str   = WorkLandline
  | isPrefixOf' "PrivateMobile" str = PrivateMobile
  | isPrefixOf' "WorkMobile" str    = WorkMobile
  | isPrefixOf' "Other" str         = Other
  | otherwise                       = error $ "Invalid phone type: " ++ str

readCountryCode str
  | isPrefixOf' "+358" str    = CountryCode (readInteger (drop 1 str))
  | isPrefixOf' "00358" str   = CountryCode (readInteger (drop 2 str))
  | isPrefixOf' "358" str     = CountryCode (readInteger str)
  | otherwise                 = error $ "Invalid country code: " ++ str



-- Helper function: readPhoneNo to read PhoneNo from string
readPhoneNo :: String -> PhoneNo
readPhoneNo str
  | all (`elem` ['0'..'9']) str = PhoneNo (readInteger str)
  | otherwise                   = error $ "Invalid phone number: " ++ str

-- Phone type
data Phone = Phone
  { phoneType :: PhoneType
  , countryCode :: CountryCode
  , phoneNo :: PhoneNo
  } deriving (Eq, Ord)

instance Show Phone where
  show (Phone pType cCode pNo) = show cCode ++ " " ++ show pNo ++ " (" ++ show pType ++ ")"

fromPhoneNo :: PhoneNo -> Integer
fromPhoneNo (PhoneNo n) = n

toPhoneNo :: Integer -> PhoneNo
toPhoneNo n
  | n < 0     = error "Negative phone number"
  | otherwise = PhoneNo n
