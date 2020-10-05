{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.ProfileRedirect where

import Import

getProfileRedirectR :: Handler ()
getProfileRedirectR = do
    uid <- requireAuthId
    mprofile <- runDB $ getBy $ UniqueProfile uid
    case mprofile of
        Nothing -> redirect HomeR
        Just (Entity profileId _) -> redirect $ ProfileR profileId ProfileDetailR
