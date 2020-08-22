{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.TableCreateEdit where

import Import

import Data.Maybe (fromJust)
import qualified Data.Text as T

data TableData = TableData
    { tableDataName :: Text
    , tableDataDescription :: Maybe Text
    }
  deriving Show

nameAttributes :: FieldSettings master
nameAttributes = FieldSettings 
    "Name*" -- The label
    Nothing -- The tooltip
    (Just "name") -- The Id
    Nothing -- The name attr
    [("class", "name")] -- list of attributes and their values

descriptionAttributes :: FieldSettings master
descriptionAttributes = FieldSettings 
    "Description" -- The label
    Nothing -- The tooltip
    Nothing -- The Id
    Nothing
    [("class", "")] -- list of attributes and their values

tableForm :: Maybe Table -> Form TableData
tableForm table = renderDivs $ TableData
    <$> areq textField nameAttributes (tableName <$> table)
    <*> aopt textField descriptionAttributes (tableDescription <$> table)

getTableCreateR :: Handler Html
getTableCreateR = do
    (widget, enctype) <- generateFormPost $ tableForm Nothing
    defaultLayout $ do
        setTitle . toHtml $ T.pack "Create new table"
        $(widgetFile "table-create")
  
postTableCreateR :: Handler ()
postTableCreateR = do
    ((result, _), _) <- runFormPost $ tableForm Nothing
    case result of
        FormSuccess tableData -> do
            tableId <- runDB $ insert $ Table
                (tableDataName tableData)
                Nothing
                (tableDataDescription tableData)
            muid <- maybeAuthId
            _ <- runDB $ insert $ Permission
                (fromJust muid)
                tableId
                Own
            redirect $ TableR tableId TableDetailR
        _ -> redirect $ TablesR TableCreateR


getTableEditR :: TableId -> Handler Html
getTableEditR tableId = do
    table <- runDB $ getJust tableId
    (widget, enctype) <- generateFormPost $ tableForm $ Just table
    defaultLayout $ do
        setTitle . toHtml $ "Update table " <> tableName table
        $(widgetFile "table-edit")
    
postTableEditR :: TableId -> Handler ()
postTableEditR tableId = do
    ((result, _), _) <- runFormPost $ tableForm Nothing
    case result of
        FormSuccess tableData -> do
            runDB $ update tableId
                [ TableName =. tableDataName tableData
                , TableDescription =. tableDataDescription tableData
                ]
            redirect $ TableR tableId TableDetailR
        _ -> redirect $ TableR tableId TableEditR
