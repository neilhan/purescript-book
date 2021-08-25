module Test.MySolutions where

import Prelude

import Data.AddressBook (AddressBook, Entry)
import Data.List (filter, head, null, nubByEq)
import Data.Maybe (Maybe)

-- Note to reader: Add your solutions to this file
findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = head <<< filter (_.address.street >>> eq street) -- filterByStreet
    -- where 
    --     filterByStreet :: Entry -> Boolean
    --     filterByStreet entry = entry.address.street == street

isInBook :: String -> String -> AddressBook -> Boolean
isInBook fname lname = 
    filter (_.firstName >>> eq fname && _.lastName >>> eq lname) >>> not null 
    -- where
    --     filterByName :: Entry -> Boolean
    --     filterByName entry = entry.firstName == fname && entry.lastName == lname

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubByEq isSame
    where 
        isSame :: Entry -> Entry -> Boolean
        isSame a b = a.firstName == b.firstName && a.lastName == b.lastName