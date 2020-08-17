{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.ColumnCreate where

import Import

getColumnCreateR :: TableId -> Handler Html
getColumnCreateR tableId = do
    (widget, enctype) <- generateFormPost columnForm
    table <- runDB $ getJust tableId
    defaultLayout $ do
        setTitle . toHtml $ "Add column to " <> tableName table
        $(widgetFile "column-create")
   
data ColumnData = ColumnData
    { columnDataName :: Text
    , columnDataDescription :: Maybe Text
    , columnDataDatatype :: Maybe Datatype
    , columnDataExample :: Maybe Text
    }
  deriving Show


columnForm :: Form ColumnData
columnForm = renderDivs $ ColumnData
    <$> areq textField "Name*" Nothing
    <*> aopt textField "Description" Nothing
    <*> pure Nothing
    <*> aopt textField "Example" Nothing

postColumnCreateR :: TableId -> Handler ()
postColumnCreateR tableId = do
    ((result, _), _) <- runFormPost columnForm
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
