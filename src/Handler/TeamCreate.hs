{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.TeamCreate where

import Import

getTeamCreateR :: Handler Html
getTeamCreateR = do
    (widget, enctype) <- generateFormPost teamForm
    defaultLayout $(widgetFile "team-create")
   
data TeamData = TeamData
    { teamDataName :: Text
    , teamDataDescription :: Maybe Text
    , teamDataPhoneNumber :: Maybe Text
    , teamDataEmailAddress :: Maybe Text
    }
  deriving Show


teamForm :: Html -> MForm Handler (FormResult TeamData, Widget)
teamForm = renderDivs $ TeamData
    <$> areq textField "Name*" Nothing
    <*> aopt textField "Description" Nothing
    <*> aopt textField "Phone Number" Nothing
    <*> aopt textField "Email Address" Nothing

postTeamCreateR :: Handler ()
postTeamCreateR = do
    ((result, _), _) <- runFormPost teamForm
    case result of
        FormSuccess teamData -> do
            teamId <- runDB $ insert $ Team
                (teamDataName teamData)
                (teamDataDescription teamData)
                (teamDataPhoneNumber teamData)
                (teamDataEmailAddress teamData)
            redirect $ TeamR teamId TeamDetailR
        _ -> redirect $ TeamsR TeamCreateR
