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
            teamId <- selectFirst [TeamName ==. "team1"] []
            profileId <- runDB $ insert $ Profile
                userId
                (profileDataName profile)
                (profileDataBio profile)
                (profileDataPhotoUrl profile)
                (fromJust teamId)

            redirect $ TeamR teamId TeamDetailR
        _ -> redirect $ TeamsR TeamCreateR


getTeamEditR :: TeamId -> Handler Html
getTeamEditR teamId = do
    team <- runDB $ getJust teamId
    (widget, enctype) <- generateFormPost $ teamForm $ Just team
    defaultLayout $ do
        setTitle . toHtml $ "Edit team " <> teamName team
        $(widgetFile "team-edit")
   
postTeamEditR :: TeamId -> Handler ()
postTeamEditR teamId = do
    ((result, _), _) <- runFormPost $ teamForm Nothing
    case result of
        FormSuccess teamData -> do
            runDB $ update teamId
                [ TeamName =. teamDataName teamData
                , TeamDescription =. teamDataDescription teamData
                , TeamPhoneNumber =. teamDataPhoneNumber teamData
                , TeamEmailAddress =. teamDataEmailAddress teamData
                ]
            redirect $ TeamR teamId TeamDetailR
        _ -> redirect $ TeamR teamId TeamEditR
