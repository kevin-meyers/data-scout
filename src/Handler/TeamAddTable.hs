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

getTeamAddTableR :: TeamId -> Handler Html
getTeamAddTableR teamId = do
    team <- runDB $ get404 teamId
    (widget, enctype) <- generateFormPost selectTableForm
    defaultLayout $ do
        setTitle . toHtml $ "Add table to " <> teamName team
        $(widgetFile "data-team-table-list")

newtype SelectedTable = SelectedTable 
    { selectedTableId :: TableId
    }

selectTableForm :: Form SelectedTable
selectTableForm = renderDivs $ SelectedTable
    <$> areq (selectField tableOptions) "Select a table" Nothing
      where
          tableOptions = optionsPersistKey [TableTeamId ==. Nothing] [] tableName

postTeamAddTableR :: TeamId -> Handler ()
postTeamAddTableR teamId = do
    ((result, _), _) <- runFormPost selectTableForm
    case result of
        FormSuccess selectData -> do
            runDB $ update (selectedTableId selectData) [TableTeamId =. Just teamId]
            redirect $ TeamR teamId TeamDetailR
        _ -> redirect $ TeamR teamId TeamAddTableR
