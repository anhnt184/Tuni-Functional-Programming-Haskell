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

-- toCountryCode function
toCountryCode :: Integer -> CountryCode
toCountryCode n
  | n < 0     = error "Negative country code"
  | otherwise = CountryCode n

-- PhoneNo type
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

-- toPhoneNo function
toPhoneNo :: Integer -> PhoneNo
toPhoneNo n
  | n < 0     = error "Negative phone number"
  | otherwise = PhoneNo n

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

-- Phone type
data Phone = Phone
  { phoneType :: PhoneType
  , countryCode :: CountryCode
  , phoneNo :: PhoneNo
  } deriving (Eq, Ord)

instance Show Phone where
  show (Phone pType cCode pNo) = show cCode ++ " " ++ show pNo ++ " (" ++ show pType ++ ")"

-- readCountryCode function
readCountryCode :: String -> CountryCode
readCountryCode str
  | '+' `elem` str = toCountryCode (readInteger (filter (\c -> c >= '0' && c <= '9') str))
  | "00" `isPrefixOf'` str = toCountryCode (readInteger (drop 2 str))
  | elem (readInteger str) predefinedCountryCodes = toCountryCode (readInteger str)
  | otherwise = error $ if null str then "Empty country code" else "Unknown country code"

-- readPhoneType function
readPhoneType :: String -> PhoneType
readPhoneType "WorkLandline" = WorkLandline
readPhoneType "PrivateMobile" = PrivateMobile
readPhoneType "WorkMobile" = WorkMobile
readPhoneType "Other" = Other
readPhoneType _ = error "Incorrect phone type"

-- readPhoneNo function
readPhoneNo :: String -> PhoneNo
readPhoneNo str
  | not (null str) && all (\c -> c >= '0' && c <= '9') str = toPhoneNo (readInteger str)
  | otherwise = error $ if null str then "Empty phone number" else "Incorrect phone number"

-- PhoneBookEntry type
data PhoneBookEntry = PhoneBookEntry { name :: String , phone :: Phone } deriving(Eq, Ord, Show)

-- PhoneBook type
type PhoneBook = [PhoneBookEntry]

-- findEntries function
findEntries :: String -> PhoneBook -> PhoneBook
findEntries query phoneBook = filter (\entry -> name entry == query) phoneBook

-- addEntry function
addEntry :: String -> String -> String -> String -> PhoneBook -> PhoneBook
addEntry name pTypeStr cCodeStr pNoStr phoneBook
  | entryExists name pNo phoneBook = phoneBook
  | otherwise = phoneBook ++ [PhoneBookEntry name (readPhone pTypeStr cCodeStr pNoStr)]
  where
    pNo = readPhoneNo pNoStr

-- Helper function: entryExists to check if an entry already exists in the phone book
entryExists :: String -> PhoneNo -> PhoneBook -> Bool
entryExists entryName pNo phoneBook = any (\entry -> entryName == name entry && pNo == phoneNo (phone entry)) phoneBook

