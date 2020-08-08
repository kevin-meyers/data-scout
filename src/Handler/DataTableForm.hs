{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.DataTableForm where

import Import

getDataTableFormR :: TableId -> ColumnId -> Handler Html
getDataTableFormR tableId columnId = do
    (widget, enctype) <- generateFormPost columnForm
    defaultLayout
        [whamlet|
            <form method=post action=@{DataTableFormR tableId columnId} enctype=#{enctype}>
                ^{widget}
                <button>Submit
            <a href=@{DataTableR tableId}>Go back!
        |]
    
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

postDataTableFormR :: TableId -> ColumnId -> Handler ()
postDataTableFormR tableId columnId = do
    ((result, _), _) <- runFormPost columnForm
    case result of
        FormSuccess columnData -> do
            runDB $ update columnId
                [ ColumnDescription =. columnDataDescription columnData
                , ColumnDatatype =. columnDataDatatype columnData
                , ColumnExample =. columnDataExample columnData
                ]
            redirect (DataTableR tableId)
        _ -> redirect (DataTableFormR tableId columnId)
