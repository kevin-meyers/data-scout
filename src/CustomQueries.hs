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
module CustomQueries where

import Import.NoFoundation

import qualified Database.Esqueleto as E
import           Database.Esqueleto      ((^.))

usedSeats :: (MonadIO m) => CompanyId -> ReaderT SqlBackend m Int
usedSeats companyId = do
    xs <- E.select $ E.from $ \(team `E.InnerJoin` profile) -> do
        E.on $ team ^. TeamId E.==. profile ^. ProfileTeamId
        E.where_ $ team ^. TeamCompanyId E.==. E.val companyId
        return E.countRows
    case xs of
        [x] -> pure $ E.unValue x
        unexpected -> error $ "Unexpected result from remainingSeats: " <> show unexpected
