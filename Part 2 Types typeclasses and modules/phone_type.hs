-- Define the PhoneType data type
data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other
  deriving (Show, Eq, Read)

-- Define the type synonyms
type CountryCode = Integer
type PhoneNo = Integer

-- Define the Phone record type
data Phone = Phone
  { phoneType :: PhoneType
  , countryCode :: CountryCode
  , phoneNo :: PhoneNo
  } deriving (Show, Eq, Read)

-- Define the makePhone function
makePhone :: PhoneType -> CountryCode -> PhoneNo -> Phone
makePhone phoneType countryCode phoneNo
  | countryCode < 0 = error "Negative country code"
  | phoneNo < 0 = error "Negative phone number"
  | otherwise = Phone phoneType countryCode phoneNo