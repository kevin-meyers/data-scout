{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.ProfileDetail where

import Import

data ProfileData = ProfileData 
    { pName :: Text
    , bio :: Maybe Text
    , photoUrl :: Maybe Text
    , tName :: Text
    , tId :: TeamId
    }

getProfileDetailR :: ProfileId -> Handler Html
getProfileDetailR profileId = do
    profile <- runDB $ get404 profileId
    uid <- requireAuthId
    let canEdit = uid == profileUserId profile
        teamId = profileTeamId profile

    team <- runDB $ get404 teamId

    defaultLayout $ do
        setTitle . toHtml $ profileName profile <> "'s User page"
        $(widgetFile "profile-detail")
