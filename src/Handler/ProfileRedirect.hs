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

module Handler.ProfileRedirect where

import Import

getProfileRedirectR :: Handler ()
getProfileRedirectR = do
    uid <- requireAuthId
    mprofile <- runDB $ selectFirst [ProfileUserId ==. uid] []
    case mprofile of
        Nothing -> redirect $ ProfilesR ProfileCreateR
        Just (Entity profileId _) -> redirect $ ProfileR profileId ProfileDetailR
