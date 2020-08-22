{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.TeamList where

import Import

getTeamListR :: Handler Html
getTeamListR = do
    --uid <- requireAuthId
    teams <- runDB $ selectList ([] :: [Filter Team]) []
    defaultLayout $ do
        setTitle . toHtml $ ("All teams" :: Text)
        $(widgetFile "team-list")
