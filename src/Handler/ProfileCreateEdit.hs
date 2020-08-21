{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.ProfileCreateEdit where

import Import

import Data.Maybe (fromJust)

data ProfileData = ProfileData
    { profileDataName :: Text
    , profileDataBio :: Maybe Text
    , profileDataPhotoUrl :: Maybe Text
    }
  deriving Show


profileForm :: Maybe Profile -> Form ProfileData
profileForm profile = renderDivs $ ProfileData
    <$> areq textField "Name*" (profileName <$> profile)
    <*> aopt textField "Bio" (profileBio <$> profile)
    <*> aopt textField "Photo Url" (profilePhotoUrl <$> profile)


getProfileCreateR :: Handler Html
getProfileCreateR = do
    (widget, enctype) <- generateFormPost $ profileForm Nothing
    defaultLayout $ do
        setTitle . toHtml $ ("Create a new profile" :: Text)
        $(widgetFile "profile-create")
   
postProfileCreateR :: Handler ()
postProfileCreateR = do
    ((result, _), _) <- runFormPost $ profileForm Nothing
    case result of
        FormSuccess profileData -> do
            userId <- requireAuthId
            mteamId <- runDB $ selectFirst [TeamName ==. "team1"] []
            let (Entity teamId _) = fromJust mteamId
            profileId <- runDB $ insert $ Profile
                (profileDataName profileData)
                userId
                (profileDataBio profileData)
                (profileDataPhotoUrl profileData)
                teamId

            redirect $ ProfileR profileId ProfileDetailR
        _ -> redirect $ ProfilesR ProfileCreateR


getProfileEditR :: ProfileId -> Handler Html
getProfileEditR profileId = do
    profile <- runDB $ getJust profileId
    (widget, enctype) <- generateFormPost $ profileForm $ Just profile
    defaultLayout $ do
        setTitle . toHtml $ "Edit " <> profileName profile <> "'s profile"
        $(widgetFile "profile-edit")
   
postProfileEditR :: ProfileId -> Handler ()
postProfileEditR profileId = do
    ((result, _), _) <- runFormPost $ profileForm Nothing
    case result of
        FormSuccess profileData -> do
            runDB $ update profileId
                [ ProfileName =. profileDataName profileData
                , ProfileBio =. profileDataBio profileData
                , ProfilePhotoUrl =. profileDataPhotoUrl profileData
                ]
            redirect $ ProfileR profileId ProfileDetailR
        _ -> redirect $ ProfileR profileId ProfileEditR
