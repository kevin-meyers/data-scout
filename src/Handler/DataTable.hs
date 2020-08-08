{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.DataTable where

import Import

getDataTableR :: TableId -> Handler Html
getDataTableR tableId = do
    columns <- runDB $ selectList [ColumnTableId ==. tableId] []
    defaultLayout
        [whamlet|
            $maybe team <- getTeam $ columnTeamId column
                <a href="url-for-team">#{teamName team}
            $nothing
                <a href="">Add your team!
            <ul>
                $forall Entity columnId column <- columns
                    <li>
                        <a href=@{DataTableFormR tableId columnId}>#{columnName column}
                        $maybe description <- columnDescription column
                            <p>#{description}
                        $maybe example <- columnExample column
                            <p>#{example}

            <a href=@{DataHomeR}>Go home
        |]
