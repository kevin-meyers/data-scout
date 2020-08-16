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
    table <- runDB $ getJust tableId
    maybeTeamEntity <- case tableTeamId table of
        Nothing -> pure Nothing
        Just teamId -> runDB $ getEntity teamId
    defaultLayout $(widgetFile "table-detail")