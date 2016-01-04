module Main

import Data.Vect

data DataStore : Type where
  MkData : (size : Nat) -> (items : Vect size String) -> DataStore

size : DataStore -> Nat
size (MkData n _) = n

items : (store : DataStore) -> Vect (size store) String
items (MkData _ items) = items

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newitem = MkData _ (addToData items)
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newitem]
    addToData (x :: xs) = x :: addToData xs


data Command = Add String | Get Integer | Search String | Size | Quit

total parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "quit" "" = Just Quit
parseCommand "size" "" = Just Size
parseCommand "add" str = Just (Add str)
parseCommand "search" str = Just (Search str)
parseCommand "get" val = case all isDigit (unpack val) of
                              False => Nothing
                              True => Just (Get (cast val))
parseCommand _ _ = Nothing


parse : String -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)

showId : (store : DataStore) -> String
showId store = "ID " ++ show (size store) ++ "\n"

showItem : (idx : Fin (size store)) -> (store_items : Vect (size store) String) -> String
showItem idx store_items = (index idx store_items) ++ "\n"

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let store_items = items store in
                         case integerToFin pos (size store) of
                              Nothing    => Just("Out of range\n", store)
                              (Just idx) => Just(showItem idx store_items, store)

||| List all the items in the datastore that start with the given string
grep : DataStore -> String -> List String
grep (MkData Z [])      _   = []
grep (MkData _ items) query = mapMaybe (match query) (toList items)
  where
    match : String -> String -> Maybe String
    match q text = if take (length (unpack q)) (unpack text) == unpack q then Just text else Nothing

showSearchResults : List String -> String
showSearchResults [] = "No results\n"
showSearchResults xs = (show xs) ++ "\n"

total processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store command = case parse command of
                                  Nothing => Just ("Invalid command\n", store)
                                  Just (Add item) => Just (showId store, addToStore store item)
                                  Just (Get pos)  => getEntry pos store
                                  Just (Search text) => Just ( showSearchResults (grep store text), store)
                                  Just Size       => Just ( (show (size store)) ++ "\n", store)
                                  Just Quit       => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput
