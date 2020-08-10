{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.DataTeamNew where

import Import

postDataTeamNewR :: Handler ()
postDataTeamNewR = do
    teamId <- runDB $ insert $ Team "" Nothing Nothing Nothing
    redirect $ DataTeamFormR teamId
