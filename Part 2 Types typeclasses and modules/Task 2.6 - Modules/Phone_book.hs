-- Phone_book module
module Phone_book
  ( Phone(..)
  , PhoneBookEntry(..)
  , readPhone
  , entryExists
  , addEntry
  , findEntries
  ) where

import Phone_type2 (Phone(..), CountryCode(..), readPhone, fromPhoneNo, toPhoneNo)


-- PhoneBookEntry type
data PhoneBookEntry = PhoneBookEntry
  { name :: String
  , phone :: Phone
  } deriving (Eq, Ord, Show)

-- PhoneBook type
type PhoneBook = [Phone_book.PhoneBookEntry]

-- findEntries function
findEntries :: String -> PhoneBook -> [Phone_book.PhoneBookEntry]
findEntries query phoneBook = filter (\entry -> name entry == query) phoneBook

addEntry :: String -> String -> String -> String -> PhoneBook -> PhoneBook
addEntry name pTypeStr cCodeStr pNoStr phoneBook
  | entryExists name p phoneBook = phoneBook
  | otherwise = phoneBook ++ [PhoneBookEntry name p]
  where
    p = readPhone pTypeStr cCodeStr pNoStr

-- Helper function: entryExists to check if an entry already exists in the phone book
entryExists :: String -> Phone -> PhoneBook -> Bool
entryExists entryName p phoneBook = any (\entry -> entryName == name entry && phoneNo (phone entry) == phoneNo p) phoneBook



