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
    { permissionDataUser :: UserId
    , permissionDataType :: PermissionType
    }

permissionForm :: Form PermissionData
permissionForm = renderDivs $ PermissionData
    <$> areq (selectField uTuples) "Select a user" Nothing
    <*> areq (selectFieldList pTuples) "which perm?" Nothing
      where
          pTuples = [("Own" :: Text, Own), ("Edit", Edit), ("View", View)]
          uTuples = optionsPersistKey ([] :: [Filter User]) [] userIdent

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
            _ <- runDB $ insert $ Permission
                (permissionDataUser permissionData)
                tableId
                (permissionDataType permissionData)
            redirect $ TableR tableId TableDetailR
        _ -> redirect $ TableR tableId TablePermissionsR
