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

module Handler.TeamDetail where

import Import

getTeamDetailR :: TeamId -> Handler Html
getTeamDetailR teamId = do
    team <- runDB $ get404 teamId
    tables <- runDB $ selectList [TableTeamId ==. teamId] []
    profiles <- runDB $ selectList [ProfileTeamId ==. teamId] []

    defaultLayout $ do
        setTitle . toHtml $ teamName team <> "'s page"
        $(widgetFile "team-detail")


