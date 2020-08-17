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
    (widget, enctype) <- generateFormPost columnForm
    column <- runDB $ getJust columnId
    table <- runDB $ getJust tableId
    defaultLayout $ do
        setTitle . toHtml $ "Update column " <> columnName column
        $(widgetFile "column-edit")
    
data ColumnData = ColumnData
    { columnDataDescription :: Maybe Text
    , columnDataDatatype :: Maybe Datatype
    , columnDataExample :: Maybe Text
    }
  deriving Show

columnForm :: Form ColumnData
columnForm = renderDivs $ ColumnData
    <$> aopt textField "Description" Nothing
    <*> pure Nothing -- aopt textField "Datatype (leave empty)" Nothing
    <*> aopt textField "Example" Nothing

postColumnEditR :: TableId -> ColumnId -> Handler ()
postColumnEditR tableId columnId = do
    ((result, _), _) <- runFormPost columnForm
    case result of
        FormSuccess columnData -> do
            runDB $ update columnId
                [ ColumnDescription =. columnDataDescription columnData
                , ColumnDatatype =. columnDataDatatype columnData
                , ColumnExample =. columnDataExample columnData
                ]
            redirect $ TableR tableId TableDetailR
        _ -> redirect $ TableR tableId $ ColumnR columnId ColumnEditR
