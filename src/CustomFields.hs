{-# LANGUAGE TemplateHaskell #-}
module CustomFields where

import Database.Persist.TH

data Datatype = Text | Numeric | Integer | Real | Blob
    deriving (Show, Read, Eq)
derivePersistField "Datatype"

