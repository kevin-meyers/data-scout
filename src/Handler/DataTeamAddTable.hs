{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.DataTeamAddTable where

import Import

postDataTeamAddTableR :: TeamId -> TableId -> Handler ()
postDataTeamAddTableR teamId tableId = do
    runDB $ update tableId [ TableTeamId =. Just teamId ]
    redirect $ TeamR teamId TeamDetailR
