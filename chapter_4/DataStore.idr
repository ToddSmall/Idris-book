module Main

import Data.Vect

data DataStore : Type where
     MkData : (size : Nat) -> (items : Vect size String) -> DataStore

size : DataStore -> Nat
size (MkData size' items') = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData size' items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newitem = MkData _ (addToData items)
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newitem]
    addToData (x :: xs) = x :: addToData xs

data Command = Add String
             | Get Integer
             | Quit
             | Search String
             | Size

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = case all isDigit (unpack val) of
                              False => Nothing
                              True => Just (Get (cast val))
parseCommand "quit" "" = Just Quit
parseCommand "search" str = Just (Search str)
parseCommand "size" "" = Just Size
parseCommand _ _ = Nothing        

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let store_items = items store in
                         case integerToFin pos (size store) of
                              Nothing => Just ("Out of range\n", store)
                              Just idx => Just (index idx store_items ++ "\n", store)

searchString : Nat -> (items : Vect n String) -> (str: String) -> String
searchString idx [] str = ""
searchString idx (x :: xs) str
    = let rest = searchString (idx + 1) xs str in
      if isInfixOf str x
         then show idx ++ ": " ++ x ++ "\n" ++ rest
         else rest   

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp = case parse inp of
                              Nothing => Just ("Invalid command\n", store)
                              Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
                              Just (Get pos) => getEntry pos store
                              Just Quit => Nothing
                              Just (Search str) => Just (searchString 0 (items store) str, store)
                              Just Size => Just ("DataStore size is " ++ show (size store) ++ "\n", store)

main : IO ()
main = replWith (MkData _ []) "Command: " processInput