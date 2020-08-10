{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.DataTeam where

import Import

getDataTeamR :: TeamId -> Handler Html
getDataTeamR teamId = do
    team <- runDB $ getJust teamId
    tables <- runDB $ selectList [TableTeamId ==. Just teamId] []
    defaultLayout
        [whamlet|
            <h1>#{teamName team}
            $maybe description <- teamDescription team
                <p>#{description}
            <p>
                $maybe phoneNumber <- teamPhoneNumber team
                    <p>The phone number is: #{phoneNumber}
                $maybe emailAddress <- teamEmailAddress team
                    <p>The email address is: #{emailAddress}

                $forall Entity tableId table <- tables
                    <a href=@{DataTableR tableId}>#{tableName table}
        |]
