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

nameAttributes :: FieldSettings master
nameAttributes = FieldSettings 
    "Name*" -- The label
    Nothing -- The tooltip
    Nothing -- The Id
    Nothing -- The name attr
    [("class", "")] -- list of attributes and their values

descriptionAttributes :: FieldSettings master
descriptionAttributes = FieldSettings 
    "Description" -- The label
    Nothing -- The tooltip
    Nothing -- The Id
    Nothing -- The name attr
    [("class", "")] -- list of attributes and their values

phoneNumberAttributes :: FieldSettings master
phoneNumberAttributes = FieldSettings 
    "Phone Number" -- The label
    Nothing -- The tooltip
    Nothing -- The Id
    Nothing -- The name attr
    [("class", "")] -- list of attributes and their values

emailAddressAttributes :: FieldSettings master
emailAddressAttributes = FieldSettings 
    "Email Address" -- The label
    Nothing -- The tooltip
    Nothing -- The Id
    Nothing
    [("class", "")] -- list of attributes and their values

teamForm :: Maybe Team -> Form TeamData
teamForm team = renderDivs $ TeamData
    <$> areq textField nameAttributes (teamName <$> team)
    <*> aopt textField descriptionAttributes (teamDescription <$> team)
    <*> aopt textField phoneNumberAttributes (teamPhoneNumber <$> team)
    <*> aopt textField emailAddressAttributes (teamEmailAddress <$> team)


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
    team <- runDB $ get404 teamId
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
