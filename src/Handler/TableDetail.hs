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

getTableDetailR :: TableId -> Handler Html
getTableDetailR tableId = do
    columns <- runDB $ selectList [ColumnTableId ==. tableId] []
    table <- runDB $ get404 tableId
    maybeTeamEntity <- case tableTeamId table of
        Nothing -> pure Nothing
        Just teamId -> runDB $ getEntity teamId
    uid <- requireAuthId
    Entity _ perm <- runDB $ getBy404 $ UniquePair uid tableId
    defaultLayout $ do
        setTitle . toHtml $ tableName table
        $(widgetFile "table-detail")
