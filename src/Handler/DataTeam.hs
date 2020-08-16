{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.DataTeam where

import Import

getDataTeamR :: TeamId -> Handler Html
getDataTeamR teamId = do
    team <- runDB $ getJust teamId
    tables <- runDB $ selectList [TableTeamId ==. Just teamId] []
    defaultLayout $(widgetFile "data-team")
