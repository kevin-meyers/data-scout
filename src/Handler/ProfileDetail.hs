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
    muid <- maybeAuthId
    let canEdit = case muid of
            Nothing -> False
            Just uid -> uid == profileUserId profile

    mteam <- case profileTeamId profile of
        Nothing -> pure Nothing
        Just teamId -> runDB $ getEntity teamId
    defaultLayout $ do
        setTitle . toHtml $ profileName profile <> "'s User page"
        $(widgetFile "profile-detail")
