{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.DataTeamTableList where

import Import

getDataTeamTableListR :: TeamId -> Handler Html
getDataTeamTableListR teamId = do
    validTableOptions <- runDB $ selectList [TableTeamId ==. Nothing] []
    defaultLayout
        [whamlet|
            $forall Entity tableId table <- validTableOptions
                <form action=@{DataTeamAddTableR teamId tableId} method="POST">
                    <input type="submit" value="add #{tableName table}">

            <a href=@{DataHomeR}>Go home
        |]
