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
    table <- runDB $ getJust tableId
    (widget, enctype) <- generateFormPost $ tableForm $ Just table
    defaultLayout $ do
        setTitle . toHtml $ "Update table " <> tableName table
        $(widgetFile "table-edit")
    
data TableData = TableData
    { tableDataName :: Text
    , tableDataDescription :: Maybe Text
    }
  deriving Show

tableForm :: Maybe Table -> Form TableData
tableForm table = renderDivs $ TableData
    <$> areq textField "Name" (tableName <$> table)
    <*> aopt textField "Description" (tableDescription <$> table)

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
