{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.TeamCreateEdit where

import Import

data TeamData = TeamData
    { teamDataName :: Text
    , teamDataDescription :: Maybe Text
    , teamDataPhoneNumber :: Maybe Text
    , teamDataEmailAddress :: Maybe Text
    }
  deriving Show


teamForm :: Maybe Team -> Form TeamData
teamForm team = renderDivs $ TeamData
    <$> areq textField "Name*" (teamName <$> team)
    <*> aopt textField "Description" (teamDescription <$> team)
    <*> aopt textField "Phone Number" (teamPhoneNumber <$> team)
    <*> aopt textField "Email Address" (teamEmailAddress <$> team)


getTeamCreateR :: Handler Html
getTeamCreateR = do
    (widget, enctype) <- generateFormPost $ teamForm Nothing
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
