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
    mProfile <- runDB $ getBy $ UniqueProfile uid
    tables <- case mProfile of
        Nothing -> return []
        Just (Entity profileId _) -> runDB
            $ E.select
            $ E.from $ \(table `E.InnerJoin` permission) -> do
                E.on $ table ^. TableId E.==. permission ^. PermissionTableId
                E.where_ $ permission ^. PermissionProfileId E.==. E.val profileId
                return
                    ( table ^. TableId
                    , table ^. TableName
                    )
    defaultLayout $ do
        setTitle . toHtml $ ("All tables" :: Text)
        $(widgetFile "table-list")
