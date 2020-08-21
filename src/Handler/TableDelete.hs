{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.TableDelete where

import Import

postTableDeleteR :: TableId -> Handler ()
postTableDeleteR tableId = do
    runDB $ delete tableId
    redirect $ TablesR TableListR
