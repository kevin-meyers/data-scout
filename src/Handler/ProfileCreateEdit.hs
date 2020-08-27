{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.ProfileCreateEdit where

import Import

data ProfileData = ProfileData
    { profileDataName :: Text
    , profileDataBio :: Maybe Text
    , profileDataPhotoUrl :: Maybe Text
    }
  deriving Show

nameAttributes :: FieldSettings master
nameAttributes = FieldSettings 
    "Name*" -- The label
    Nothing -- The tooltip
    Nothing -- The Id
    Nothing
    [("class", "profileName inputField")] -- list of attributes and their values

bioAttributes :: FieldSettings master
bioAttributes = FieldSettings 
    "Bio" -- The label
    Nothing -- The tooltip
    Nothing -- The Id
    Nothing -- The name attr
    [("class", "")] -- list of attributes and their values

photoUrlAttributes :: FieldSettings master
photoUrlAttributes = FieldSettings 
    "Photo Url" -- The label
    Nothing -- The tooltip
    Nothing -- The Id
    Nothing -- The name attr
    [("class", "")] -- list of attributes and their values

profileForm :: Maybe Profile -> Form ProfileData
profileForm profile = renderDivs $ ProfileData
    <$> areq textField nameAttributes (profileName <$> profile)
    <*> aopt textField bioAttributes (profileBio <$> profile)
    <*> aopt textField photoUrlAttributes (profilePhotoUrl <$> profile)

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
            profileId <- runDB $ insert $ Profile
                (profileDataName profileData)
                userId
                (profileDataBio profileData)
                (profileDataPhotoUrl profileData)
                Nothing

            redirect $ ProfileR profileId ProfileDetailR
        _ -> redirect $ ProfilesR ProfileCreateR


getProfileEditR :: ProfileId -> Handler Html
getProfileEditR profileId = do
    profile <- runDB $ get404 profileId
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
