{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.DataTableForm where

import Import
import qualified Data.Text as T

getDataTableFormR :: ColumnId -> Handler Html
getDataTableFormR columnId = do
    (widget, enctype) <- generateFormPost columnForm
    column <- runDB $ getJust columnId
    let tableId = columnTableId column
    defaultLayout $ do
        setTitle . toHtml $ T.pack "Update column"
        $(widgetFile "data-table-form")
    
data ColumnData = ColumnData
    { columnDataDescription :: Maybe Text
    , columnDataDatatype :: Maybe Datatype
    , columnDataExample :: Maybe Text
    }
  deriving Show

columnForm ::  Html -> MForm Handler (FormResult ColumnData, Widget)
columnForm = renderDivs $ ColumnData
    <$> aopt textField "Description" Nothing
    <*> pure Nothing -- aopt textField "Datatype (leave empty)" Nothing
    <*> aopt textField "Example" Nothing

postDataTableFormR :: ColumnId -> Handler ()
postDataTableFormR columnId = do
    ((result, _), _) <- runFormPost columnForm
    column <- runDB $ getJust columnId
    let tableId = columnTableId column
    case result of
        FormSuccess columnData -> do
            runDB $ update columnId
                [ ColumnDescription =. columnDataDescription columnData
                , ColumnDatatype =. columnDataDatatype columnData
                , ColumnExample =. columnDataExample columnData
                ]
            redirect (DataTableR tableId)
        _ -> redirect (DataTableFormR columnId)
