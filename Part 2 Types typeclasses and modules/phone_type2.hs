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
toCountryCode n = fromInteger n

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
toPhoneNo n = fromInteger n

-- PhoneType type
data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Eq, Ord, Read, Show)

-- Phone type
data Phone = Phone
  { phoneType :: PhoneType
  , countryCode :: CountryCode
  , phoneNo :: PhoneNo
  } deriving (Eq, Ord)

instance Show Phone where
  show (Phone pType cCode pNo) = show cCode ++ " " ++ show pNo ++ " (" ++ show pType ++ ")"

-- makePhone function
makePhone :: PhoneType -> CountryCode -> PhoneNo -> Phone
makePhone pType cCode pNo = Phone pType cCode pNo

-- -- main function
-- main :: IO ()
-- main = do
--   input1 <- getLine
--   input2 <- getLine
--   input3 <- getLine

--   let pType = read input1
--       cCode = toCountryCode (read input2)
--       pNo = toPhoneNo (read input3)

--       invalidInput = any (not . (`elem` ['0'..'9'])) input2 || any (not . (`elem` ['0'..'9'])) input3

--   if invalidInput
--     then error "Invalid input"
--     else print $ makePhone pType cCode pNo