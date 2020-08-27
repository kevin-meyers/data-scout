{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.TableList where

import Import

import qualified Database.Esqueleto as E
import           Database.Esqueleto      ((^.))

getTableListR :: Handler Html
getTableListR = do
    uid <- requireAuthId
    tables <- runDB 
        $ E.select
        $ E.from $ \(table `E.InnerJoin` permission) -> do
            E.on $ table ^. TableId E.==. permission ^. PermissionTableId
            E.where_ $ permission ^. PermissionUserId E.==. E.val uid
            return
                ( table ^. TableId
                , table ^. TableName
                )
    defaultLayout $ do
        setTitle . toHtml $ ("All tables" :: Text)
        $(widgetFile "table-list")
