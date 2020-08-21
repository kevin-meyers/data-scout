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
    profile <- runDB $ getJust profileId
    muid <- maybeAuthId
    let canEdit = case muid of
            Nothing -> False
            Just uid -> uid == profileUserId profile

    (Entity teamId team) <- runDB $ getJustEntity $ profileTeamId profile
    defaultLayout $ do
        setTitle . toHtml $ profileName profile <> "'s User page"
        $(widgetFile "profile-detail")
