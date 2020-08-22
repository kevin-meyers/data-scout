{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.TeamAddTable where

import Import


newtype SelectedTable = SelectedTable 
    { selectedTableId :: TableId
    }

tableAttributes :: FieldSettings master
tableAttributes = FieldSettings 
    "Select a table" -- The label
    Nothing -- The tooltip
    Nothing -- The Id
    Nothing -- The name attr
    [("class", "")] -- list of attributes and their values

selectTableForm :: Form SelectedTable
selectTableForm = renderDivs $ SelectedTable
    <$> areq (selectField tableOptions) tableAttributes Nothing
      where
          tableOptions = optionsPersistKey [TableTeamId ==. Nothing] [] tableName

getTeamAddTableR :: TeamId -> Handler Html
getTeamAddTableR teamId = do
    team <- runDB $ get404 teamId
    (widget, enctype) <- generateFormPost selectTableForm
    defaultLayout $ do
        setTitle . toHtml $ "Add table to " <> teamName team
        $(widgetFile "data-team-table-list")

postTeamAddTableR :: TeamId -> Handler ()
postTeamAddTableR teamId = do
    ((result, _), _) <- runFormPost selectTableForm
    case result of
        FormSuccess selectData -> do
            runDB $ update (selectedTableId selectData) [TableTeamId =. Just teamId]
            redirect $ TeamR teamId TeamDetailR
        _ -> redirect $ TeamR teamId TeamAddTableR
