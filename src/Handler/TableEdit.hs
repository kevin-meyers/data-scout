{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.TableEdit where

import Import

getTableEditR :: TableId -> Handler Html
getTableEditR tableId = do
    (widget, enctype) <- generateFormPost tableForm
    table <- runDB $ getJust tableId
    defaultLayout $ do
        setTitle . toHtml $ "Update table " <> tableName table
        $(widgetFile "table-edit")
    
data TableData = TableData
    { tableDataName :: Text
    , tableDataDescription :: Maybe Text
    }
  deriving Show

tableForm :: Form TableData
tableForm = renderDivs $ TableData
    <$> areq textField "Name" Nothing
    <*> aopt textField "Description" Nothing

postTableEditR :: TableId -> Handler ()
postTableEditR tableId = do
    ((result, _), _) <- runFormPost tableForm
    case result of
        FormSuccess tableData -> do
            runDB $ update tableId
                [ TableName =. tableDataName tableData
                , TableDescription =. tableDataDescription tableData
                ]
            redirect $ TableR tableId TableDetailR
        _ -> redirect $ TableR tableId TableEditR
