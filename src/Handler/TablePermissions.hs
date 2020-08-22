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

module Handler.TablePermissions where

import Import

data PermissionData = PermissionData 
    { permissionDataProfileId :: ProfileId
    , permissionDataType :: PermissionType
    }

permissionForm :: Form PermissionData
permissionForm = renderDivs $ PermissionData
    <$> areq (selectField uTuples) "Select a user" Nothing
    <*> areq (selectFieldList pTuples) "which perm?" Nothing
      where
          pTuples = [("Own" :: Text, Own), ("Edit", Edit), ("View", View)]
          uTuples = optionsPersistKey ([] :: [Filter Profile]) [] profileName

getTablePermissionsR :: TableId -> Handler Html
getTablePermissionsR tableId = do
    table <- runDB $ getJust tableId
    (widget, enctype) <- generateFormPost permissionForm
    defaultLayout $ do
        setTitle . toHtml $ "Add permissions for table: " <> tableName table
        $(widgetFile "table-permissions")

postTablePermissionsR :: TableId -> Handler ()
postTablePermissionsR tableId = do
    ((result, _), _) <- runFormPost permissionForm
    case result of
        FormSuccess permissionData -> do
            profile <- runDB $ get404 $ permissionDataProfileId permissionData
            _ <- runDB $ insert $ Permission
                (profileUserId profile)
                tableId
                (permissionDataType permissionData)
            redirect $ TableR tableId TableDetailR
        _ -> redirect $ TableR tableId TablePermissionsR
