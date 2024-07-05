{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Control.CRDT.JsonDoc where

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Text.RawString.QQ (r)

import qualified Data.Text as DT

emptyJsonObject :: String
emptyJsonObject = [r|{}|]

emptyJsonArray :: String
emptyJsonArray = [r|[]|]

jsonObject :: String
jsonObject = [r|
  {
    "name": "Jack Sparrow",
    "rank": "Captain"
  }
|]

jsonArray :: String
jsonArray = [r|
  [
    "North",
    "East",
    "South",
    "West"
  ]
|]


addKeyValue :: String
addKeyValue = emptyJsonObject & _Object . at "key01" ?~ "50.0" --key "key01" ?~ "50.0"
                              & _Object . at "key02" ?~ "true"
                              & _Object . at "key03" ?~ "\"John\""

fleet :: String
fleet = [r|
  [{"name": "Black Pearl",
    "crew": [{"name": "Jack Sparrow", "rank": "Captain", "age": 60},
             {"name": "Will Turner",  "rank": "First Mate", "age": 50}]},
   {"name": "Flying Dutchman",
    "crew": [{"name": "Davy Jones", "rank": "Captain", "age": 65},
             {"name": "Bootstrap Bill", "rank": "First Mate", "age": 55}]}]|]

cargo :: String
cargo = [r|{"emeralds": 327, "rubies": 480, "sapphires": 621, "opals": 92, "dubloons": 34}|]

fleet1 :: String
fleet1 = [r|
  [{"name": "Black Pearl",
    "crew": [{"name": "Jack Sparrow", "rank": "Captain",    "age": 60, "married": true},
             {"name": "Will Turner",  "rank": "First Mate", "age": 50, "married": false}]}]|]

{-
-- assign
-- insert
-- delete
-- there should be an option : ResetCRDT wins or Other ops wins HPReset, LPReset?
-- we should be able to reset JsonDoc at any sub-tree
-- any operation at higher level of a sub-tree will have precedence over a lower level sub-tree
-}
