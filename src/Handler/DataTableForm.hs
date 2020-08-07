{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.DataTableForm where

import Import

getDataTableFormR :: Handler Html
getDataTableFormR = do
    (widget, enctype) <- generateFormPost columnForm
    defaultLayout
        [whamlet|
            <form method=post action=@{DataTableFormR} enctype=#{enctype}>
                ^{widget}
                <button>Submit
        |]
    
data ColumnData = ColumnData
    { columnId :: ColumnId
    , columnDataDescription :: Maybe Text
    , columnDataDatatype :: Maybe Datatype
    , columnDataExample :: Maybe Text
    }
  deriving Show

columnForm :: ColumnId -> Html -> MForm Handler (FormResult ColumnData, Widget)
columnForm columnId = renderDivs $ ColumnData
    <$> pure columnId
    <*> aopt textField "Description" Nothing
    <*> aopt textField "Datatype (leave empty)" Nothing
    <*> aopt textField "Example" Nothing

postMetadataFormR :: Handler Html
postMetadataFormR = do
    ((result, _), _) <- runFormPost columnForm
    case result of
        FormSuccess columnData -> 
            update (columnId columnData)
                [ ColumnDescription =. columnDataDescription columnData
                , ColumnDatatype =. columnDataDatatype columnData
                , ColumnExample =. columnDataExample columnData
                ]
        _ -> defaultLayout
            [whamlet|
                <p>Invalid input, let's try again
            |]
