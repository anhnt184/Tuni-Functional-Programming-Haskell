module Phone_book_map
  ( Phonebook
  , addEntry
  , findEntries
  ) where

import qualified Data.Map as Map
import Phone_type2

type Name = String
type Phonebook = Map.Map Name [Phone]

addEntry :: Name -> String -> String -> String -> Phonebook -> Phonebook
addEntry name pTypeStr cCodeStr pNoStr phonebook =
  let p = readPhone pTypeStr cCodeStr pNoStr
      entry = case Map.lookup name phonebook of
        Nothing -> [p]
        Just phones -> if any (\phone -> countryCode phone == countryCode p && phoneNo phone == phoneNo p) phones
                       then phones
                       else p : phones
  in Map.insert name entry phonebook


findEntries :: Name -> Phonebook -> [Phone]
findEntries name phonebook = Map.findWithDefault [] name phonebook
