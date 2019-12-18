module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, null)
import Data.Maybe (Maybe)



type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type AddressBook = List Entry

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <> addr.city <> ", " <> addr.state

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <> entry.firstName <> ": " <> showAddress entry.address

emptyBook :: AddressBook
emptyBook = empty


-- showNames :: AddressBook -> String
-- showNames = firstName lastName = head <<< filter filterEntry
--   where
--   filterEntry :: Entry -> Boolean  --filter type is (Entry -> Boolean) -> List a -> List a: = filter filerEntry List a
--   filterEntry entry = entry.firstName == firstName && entry.lastName == lastName



insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons
--insertEntry entry book = Cons entry book

findEntry :: String -> String -> AddressBook -> Maybe Entry  
--findEntry firstName lastName book = head $filter filterEntry book  --  (Entry -> Boolean) -> List a -> List a -> Maybe a
--findEntry firstName lastName = filter filterEntry >>> head -- head :: forall a. List a -> Maybe a
findEntry firstName lastName = head <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean  --filter type is (Entry -> Boolean) -> List a -> List a: = filter filerEntry List a
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName


findEntrywithStreet :: String -> AddressBook -> Maybe Entry  
--findEntry firstName lastName book = head $filter filterEntry book  --  (Entry -> Boolean) -> List a -> List a -> Maybe a
--findEntry firstName lastName = filter filterEntry >>> head -- head :: forall a. List a -> Maybe a
findEntrywithStreet street = head <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean  --filter type is (Entry -> Boolean) -> List a -> List a: = filter filerEntry List a
  filterEntry entry = entry.address.street == street



printEntry :: String -> String -> AddressBook ->  Maybe String
printEntry firstName lastName book = map showEntry (findEntry firstName lastName book)


testEntry :: String -> AddressBook -> Maybe Entry  
testEntry name book  = head $filter filterEntry book
  where
  filterEntry :: Entry -> Boolean  --filter type is (Entry -> Boolean) -> List a -> List a: = filter filerEntry List a
  filterEntry entry = entry.firstName == name || entry.lastName == name

checkEntry :: String -> AddressBook ->  Boolean
checkEntry name book = null $filter filterEntry book
  where
  filterEntry :: Entry -> Boolean  --filter type is (Entry -> Boolean) -> List a -> List a: = filter filerEntry List a
  filterEntry entry = entry.firstName == name || entry.lastName == name


testEntry1 :: String -> AddressBook -> Boolean
testEntry1 name book = checkExist $filter filterEntry book
  where
  filterEntry :: Entry -> Boolean  --filter type is (Entry -> Boolean) -> List a -> List a: = filter filerEntry List a
  filterEntry entry = entry.firstName == name || entry.lastName == name
  
filterEntry1 :: String -> Entry -> Boolean  --filter type is (Entry -> Boolean) -> List a -> List a: = filter filerEntry List a
filterEntry1 name entry = entry.firstName == name || entry.lastName == name

checkExist :: AddressBook -> Boolean
checkExist Nil = false                
checkExist book = true