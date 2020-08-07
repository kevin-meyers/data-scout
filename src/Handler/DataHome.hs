{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.DataHome where

import Import

getDataHomeR :: Handler Html
getDataHomeR = do
    tables <- runDB $ selectList ([] :: [Filter Table]) []
    defaultLayout
        [whamlet|
            <ul>
                $forall Entity tableId table <- tables
                    <li>
                        <a href=@{DataTableR tableId}>#{tableName table}
        |]

 --insert $ Person "Kevin" "Meyers" 21
