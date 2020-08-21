{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.ColumnCreateEdit where

import Import

data ColumnData = ColumnData
    { columnDataName :: Text
    , columnDataDescription :: Maybe Text
    , columnDataDatatype :: Maybe Datatype
    , columnDataExample :: Maybe Text
    }
  deriving Show

nameAttributes :: FieldSettings master
nameAttributes = FieldSettings 
    "Name*" -- The label
    Nothing -- The tooltip
    Nothing -- The Id
    (Just "Name") -- The name attr
    [("class", "")] -- list of attributes and their values

descriptionAttributes :: FieldSettings master
descriptionAttributes = FieldSettings 
    "Description" -- The label
    Nothing -- The tooltip
    Nothing -- The Id
    (Just "Name") -- The name attr
    [("class", "")] -- list of attributes and their values

exampleAttributes :: FieldSettings master
exampleAttributes = FieldSettings 
    "Example" -- The label
    Nothing -- The tooltip
    Nothing -- The Id
    (Just "Name") -- The name attr
    [("class", "")] -- list of attributes and their values

columnForm :: Maybe Column -> Form ColumnData
columnForm column = renderDivs $ ColumnData
    <$> areq textField nameAttributes (columnName <$> column)
    <*> aopt textField descriptionAttributes (columnDescription <$> column)
    <*> pure Nothing -- aopt textField "Datatype (leave empty)" Nothing
    <*> aopt textField exampleAttributes (columnExample <$> column)


getColumnEditR :: TableId -> ColumnId -> Handler Html
getColumnEditR tableId columnId = do
    column <- runDB $ getJust columnId
    table <- runDB $ getJust tableId
    (widget, enctype) <- generateFormPost $ columnForm $ Just column
    defaultLayout $ do
        setTitle . toHtml $ "Update column " <> columnName column
        $(widgetFile "column-edit")

postColumnEditR :: TableId -> ColumnId -> Handler ()
postColumnEditR tableId columnId = do
    ((result, _), _) <- runFormPost $ columnForm Nothing
    case result of
        FormSuccess columnData -> do
            runDB $ update columnId
                [ ColumnName =. columnDataName columnData
                , ColumnDescription =. columnDataDescription columnData
                , ColumnDatatype =. columnDataDatatype columnData
                , ColumnExample =. columnDataExample columnData
                ]
            redirect $ TableR tableId TableDetailR
        _ -> redirect $ TableR tableId $ ColumnR columnId ColumnEditR


getColumnCreateR :: TableId -> Handler Html
getColumnCreateR tableId = do
    (widget, enctype) <- generateFormPost $ columnForm Nothing
    table <- runDB $ getJust tableId
    defaultLayout $ do
        setTitle . toHtml $ "Add column to " <> tableName table
        $(widgetFile "column-create")
   

postColumnCreateR :: TableId -> Handler ()
postColumnCreateR tableId = do
    ((result, _), _) <- runFormPost $ columnForm Nothing
    case result of
        FormSuccess columnData -> do
            _ <- runDB $ insert $ Column
                (columnDataName columnData)
                (columnDataDescription columnData)
                Nothing
                (columnDataExample columnData)
                tableId
            redirect $ TableR tableId TableDetailR
        _ -> redirect $ TableR tableId $ ColumnsR ColumnCreateR

