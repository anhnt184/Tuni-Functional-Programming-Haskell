-- read_phone.hs

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

-- predefined country codes
predefinedCountryCodes :: [Integer]
predefinedCountryCodes = [1, 44, 45, 46, 47, 48, 49, 51, 52, 53, 54, 55, 56, 57, 58, 60, 61, 62, 63, 64, 65, 66, 81, 82, 84, 86, 90, 91, 92, 93, 94, 95, 98, 211, 212, 213, 216, 218, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255, 256, 257, 258, 260, 261, 262, 263, 264, 265, 266, 267, 268, 269, 290,291, 297, 298, 299, 350, 351, 352, 353, 354, 355, 356, 357, 358, 359, 370]

-- readPhone function
readPhone :: String -> String -> String -> Phone
readPhone pt cc pn = case pt of
readPhone :: String -> String -> String -> Phone
readPhone phoneTypeStr countryCodeStr phoneNoStr =
  let
    phoneType = case phoneTypeStr of
      "WorkLandline"  -> WorkLandline
      "PrivateMobile" -> PrivateMobile
      "WorkMobile"    -> WorkMobile
      ""              -> error "Missing phone type"
      _               -> error "Incorrect phone type"

    strippedCountryCodeStr = case stripPrefix "+" countryCodeStr of
      Just rest           -> rest
      Nothing             -> case stripPrefix "00" countryCodeStr of
        Just rest'         -> rest'
        Nothing            -> countryCodeStr

    readCountryCode = case reads strippedCountryCodeStr of
      [(n, "")] -> if n `elem` predefinedCountryCodes
                      then toCountryCode n
                      else error "Unknown country code"
      _         -> error "Incorrect country code"

    readPhoneNo = case reads phoneNoStr of
      [(n, "")] -> toPhoneNo n
      _         -> error "Incorrect phone number"
  in
    makePhone phoneType readCountryCode readPhoneNo
