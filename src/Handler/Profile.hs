{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Profile where

import Import

data ProfileData = ProfileData 
    { pName :: Text
    , bio :: Maybe Text
    , photoUrl :: Maybe Text
    , tName :: Text
    , tId :: TeamId
    }

getProfileR :: ProfileId -> Handler Html
getProfileR profileId = do
    mprofile <- runDB $ get profileId
    case mprofile of
        Nothing -> do
            setUltDest $ ProfileR profileId ProfileDetailR
            redirect $ ProfileR profileId ProfileEditR
        Just (Entity _ profile) -> do
            team <- runDB $ getJust $ profileTeamId profile
            defaultLayout $ do
                setTitle . toHtml $ profileName profile <> "'s User page"
                $(widgetFile "profile")
