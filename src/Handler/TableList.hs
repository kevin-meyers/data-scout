{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.TableList where

import Import

import qualified Database.Esqueleto as E
import           Database.Esqueleto      ((^.))

import Data.Maybe (fromJust)

getTableListR :: Handler Html
getTableListR = do
    muid <- maybeAuthId
    tables <- runDB 
        $ E.select
        $ E.from $ \(table `E.InnerJoin` permission) -> do
            E.on $ table ^. TableId E.==. permission ^. PermissionTableId
            E.where_ $ permission ^. PermissionUserId E.==. E.val (fromJust muid)
            return
                ( table ^. TableId
                , table ^. TableName
                )
    defaultLayout $(widgetFile "table-list")
