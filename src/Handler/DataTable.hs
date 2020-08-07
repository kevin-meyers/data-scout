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
            <ul>
                $forall Entity _ column <- columns
                    <li>
                        #{columnName column}
        |]
