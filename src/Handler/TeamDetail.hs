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
    uid <- requireAuthId
    team <- runDB $ get404 teamId
    profileId <- do
        mprofile <- runDB $ getBy $ UniqueProfile uid
        maybe notFound (pure . entityKey) mprofile

    tables <- runDB
        $ E.select
        $ E.from $ \(table `E.InnerJoin` permission) -> do
            E.on $ table ^. TableId E.==. permission ^. PermissionTableId
            E.where_ $ permission ^. PermissionProfileId E.==. E.val profileId
            E.where_ $ table ^. TableTeamId E.==. E.val teamId
            return
                ( table ^. TableId
                , table ^. TableName
                )

    profiles <- runDB $ selectList [ProfileTeamId ==. teamId] []

    defaultLayout $ do
        setTitle . toHtml $ teamName team <> "'s page"
        $(widgetFile "team-detail")


