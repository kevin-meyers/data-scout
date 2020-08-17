{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.TableDetail where

import Import

import Data.Maybe (fromJust)

getTableDetailR :: TableId -> Handler Html
getTableDetailR tableId = do
    columns <- runDB $ selectList [ColumnTableId ==. tableId] []
    table <- runDB $ getJust tableId
    maybeTeamEntity <- case tableTeamId table of
        Nothing -> pure Nothing
        Just teamId -> runDB $ getEntity teamId
    muid <- maybeAuthId
    mperm <- runDB $ getBy $ UniquePair (fromJust muid) tableId
    let (Entity _ perm) = fromJust mperm
    defaultLayout $ do
        setTitle . toHtml $ tableName table
        $(widgetFile "table-detail")
