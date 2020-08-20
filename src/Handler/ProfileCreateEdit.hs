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
    , profileDatabio :: Maybe Text
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
        setTitle . toHtml $ ("Create a team" :: Text)
        $(widgetFile "team-create")
   
postTeamCreateR :: Handler ()
postTeamCreateR = do
    ((result, _), _) <- runFormPost $ teamForm Nothing
    case result of
        FormSuccess teamData -> do
            teamId <- runDB $ insert $ Team
                (teamDataName teamData)
                (teamDataDescription teamData)
                (teamDataPhoneNumber teamData)
                (teamDataEmailAddress teamData)
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
