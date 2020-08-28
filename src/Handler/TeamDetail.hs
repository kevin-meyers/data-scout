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

import qualified Database.Esqueleto as E
import           Database.Esqueleto      ((^.))

getTeamDetailR :: TeamId -> Handler Html
getTeamDetailR teamId = do
    team <- runDB $ get404 teamId
    tables <- runDB $ selectList [TableTeamId ==. Just teamId] []
    profiles <- runDB
        $ E.select
        $ E.from $ \(user `E.InnerJoin` profile) -> do
            E.on $ user ^. UserId E.==. profile ^. ProfileUserId
            E.where_ $ user ^. UserTeamId E.==. E.val (Just teamId)
            return profile

    defaultLayout $ do
        setTitle . toHtml $ teamName team <> "'s page"
        $(widgetFile "team-detail")


