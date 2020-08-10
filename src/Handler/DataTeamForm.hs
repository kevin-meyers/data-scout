{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.DataTeamForm where

import Import

getDataTeamFormR :: TeamId -> Handler Html
getDataTeamFormR teamId = do
    (widget, enctype) <- generateFormPost teamForm
    defaultLayout
        [whamlet|
            <form method=post action=@{DataTeamFormR teamId} enctype=#{enctype}>
                ^{widget}
                <button>Submit
            <a href=@{DataHomeR}>Go Home!
        |]
    
data TeamData = TeamData
    { teamDataName :: Text
    , teamDataDescription :: Maybe Text
    , teamDataPhoneNumber :: Maybe Text
    , teamDataEmailAddress :: Maybe Text
    }
  deriving Show

teamForm ::  Html -> MForm Handler (FormResult TeamData, Widget)
teamForm = renderDivs $ TeamData
    <$> areq textField "Name*" Nothing
    <*> aopt textField "Description" Nothing
    <*> aopt textField "Phone Number" Nothing
    <*> aopt textField "Email Address" Nothing

postDataTeamFormR :: TeamId -> Handler ()
postDataTeamFormR teamId = do
    ((result, _), _) <- runFormPost teamForm
    case result of
        FormSuccess teamData -> do
            runDB $ update teamId
                [ TeamName =. teamDataName teamData
                , TeamDescription =. teamDataDescription teamData
                , TeamPhoneNumber =. teamDataPhoneNumber teamData
                , TeamEmailAddress =. teamDataEmailAddress teamData
                ]
            redirect DataHomeR
        _ -> redirect (DataTeamFormR teamId)
