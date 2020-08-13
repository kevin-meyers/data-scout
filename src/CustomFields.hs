{-# LANGUAGE TemplateHaskell #-}
module CustomFields where

import Database.Persist.TH

data Datatype = Text | Numeric | Integer | Real | Blob
    deriving (Show, Read, Eq)
derivePersistField "Datatype"

data PermissionType = View | Edit | Own
    deriving (Show, Read, Eq, Ord)
derivePersistField "PermissionType"
