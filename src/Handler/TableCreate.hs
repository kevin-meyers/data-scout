{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.TableCreate where

import Import
import Data.Maybe (fromJust)

getTableCreateR :: Handler Html
getTableCreateR = do
    (widget, enctype) <- generateFormPost tableForm
    defaultLayout $(widgetFile "table-create")
   
data TableData = TableData
    { tableDataName :: Text
    , tableDataDescription :: Maybe Text
    }
  deriving Show


tableForm :: Form TableData
tableForm = renderDivs $ TableData
    <$> areq textField "Name*" Nothing
    <*> aopt textField "Description" Nothing

postTableCreateR :: Handler ()
postTableCreateR = do
    ((result, _), _) <- runFormPost tableForm
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
