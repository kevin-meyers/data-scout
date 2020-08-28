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
    (uid, user) <- requireAuthPair
    let canEdit = uid == profileUserId profile

    mteam <- case userTeamId user of
        Nothing -> pure Nothing
        Just teamId -> runDB $ getEntity teamId
    defaultLayout $ do
        setTitle . toHtml $ profileName profile <> "'s User page"
        $(widgetFile "profile-detail")
