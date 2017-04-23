module Data.AddressBook where

import Prelude
import Partial.Unsafe(unsafePartial)
import Control.Plus (empty)
import Data.List (List(..), filter, head,null)
import Data.Maybe (Maybe)

type Entry = 
  {firstName :: String
  ,lastName  :: String
  ,address   :: Address
  }
  
type Address = 
  {street :: String
  ,city   :: String
  ,state  :: String
  }

type AddressBook = List Entry

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <>
                  entry.firstName <> ": " <>
                  showAddress entry.address

showAddress :: Address -> String
showAddress addr = addr.street <>  ", " <>
                   addr.city <> ", " <>
                   addr.state

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry->AddressBook->AddressBook
insertEntry = Cons

entriesMatching :: String->String->AddressBook->List Entry
entriesMatching firstName lastName = filter filterEntry
  where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

findEntry :: String->String->AddressBook->Maybe Entry

findEntry firstName lastName = head <<< entriesMatching firstName lastName

hasEntry :: String->String->AddressBook->Boolean
hasEntry f l  = entriesMatching  f l >>> null >>>not

--x#type Person = { name :: String, address :: Address }

--sameCity :: {address :: {city::String}} ->{address :: {city::String}}->Boolean
--sameCity {address:{city:c1}} {address:{city:c2}}  = c1 == c2
fromSingleton:: forall a. a->Array a->a
fromSingleton dfl [] = dfl
fromSingleton dfl [x] = x
fromSingleton dfl _ = dfl



-- test data for PSCI
address :: Address
address = {street: "3 the valley", city:"radford semele", state:"warwickshire"}
entry :: Entry
entry = {firstName: "chris", lastName: "may", address: address}
b :: AddressBook
b=insertEntry entry emptyBook

