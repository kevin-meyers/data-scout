{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.ColumnEdit where

import Import

getColumnEditR :: TableId -> ColumnId -> Handler Html
getColumnEditR tableId columnId = do
    column <- runDB $ getJust columnId
    table <- runDB $ getJust tableId
    (widget, enctype) <- generateFormPost $ columnForm $ Just column
    defaultLayout $ do
        setTitle . toHtml $ "Update column " <> columnName column
        $(widgetFile "column-edit")
    
data ColumnData = ColumnData
    { columnDataName :: Text
    , columnDataDescription :: Maybe Text
    , columnDataDatatype :: Maybe Datatype
    , columnDataExample :: Maybe Text
    }
  deriving Show

columnForm :: Maybe Column -> Form ColumnData
columnForm column = renderDivs $ ColumnData
    <$> areq textField "Name" (columnName <$> column)
    <*> aopt textField "Description" (columnDescription <$> column)
    <*> pure Nothing -- aopt textField "Datatype (leave empty)" Nothing
    <*> aopt textField "Example" (columnExample <$> column)

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
