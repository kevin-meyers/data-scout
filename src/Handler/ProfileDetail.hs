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
    team <- runDB $ getJust $ profileTeamId profile
    defaultLayout $ do
        setTitle . toHtml $ profileName profile <> "'s User page"
        $(widgetFile "profile")
