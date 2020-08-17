{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.ColumnDelete where

import Import

postColumnDeleteR :: TableId -> ColumnId -> Handler ()
postColumnDeleteR tableId columnId = do
    runDB $ delete columnId
    redirect $ TableR tableId TableDetailR
