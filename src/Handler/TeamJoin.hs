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

module Handler.TeamJoin where

import Import

getTeamJoinR :: TeamId -> Handler Html
getTeamJoinR teamId = do
    team <- runDB $ get404 teamId
    defaultLayout $ do
        setTitle . toHtml $ "Join team: " <> teamName team <> "?"
        $(widgetFile "team-join")

postTeamJoinR :: TeamId -> Handler ()
postTeamJoinR teamId = do
    userId <- requireAuthId

    runDB $ update profileId [UserTeamId =. Just teamId]
    redirect $ TeamR teamId ProfileCreateR
