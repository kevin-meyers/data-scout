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

module Handler.DataTeamTableList where

import Import

getDataTeamTableListR :: TeamId -> Handler Html
getDataTeamTableListR teamId = do
    validTableOptions <- runDB $ selectList [TableTeamId ==. Nothing] []
    team <- runDB $ getJust teamId
    defaultLayout $(widgetFile "data-team-table-list")
