{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Control.Monad.Logger (LogSource)

import Yesod.Auth.OAuth2.Google
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T

-- Replace with Google client ID.
clientId :: Text
clientId = "116467907892-u186h8n1tec5lb7alru80gjp6isk4n9f.apps.googleusercontent.com"

-- Replace with Google secret ID.
clientSecret :: Text
clientSecret = "jMSGqDcwnzQFSBt6-YJAVB0E"

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

data MenuItem = MenuItem
    { menuItemLabel :: Text
    , menuItemRoute :: Route App
    , menuItemAccessCallback :: Bool
    }

data MenuTypes
    = NavbarLeft MenuItem
    | NavbarRight MenuItem

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerFor App
-- type Widget = WidgetFor App ()
mkYesodData "App" $(parseRoutesFile "config/routes.yesodroutes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a = forall (m :: * -> *).
    (MonadUnliftIO m) => ReaderT SqlBackend m a

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot :: Approot App
    approot = ApprootRequest $ \app req ->
        fromMaybe (getApprootText guessApproot app req) (appRoot $ appSettings app)

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
    yesodMiddleware = defaultYesodMiddleware

    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        muser <- maybeAuthPair
        mProfile <- maybe (pure Nothing) (runDB . getBy . UniqueProfile . fst) muser

        let mTeamId = profileTeamId . entityVal <$> mProfile

        mCompanyId <- case mTeamId of
            Nothing -> case muser of
                Nothing -> pure Nothing
                Just (uid, _) -> do
                    mCompanyAdmin <- runDB $ getBy $ UniqueAdmin uid
                    pure $ entityKey <$> mCompanyAdmin

            Just teamId -> do
                mteam <- runDB $ get teamId
                case mteam of
                    Nothing -> pure Nothing
                    Just team -> pure $ Just $ teamCompanyId team

        mcurrentRoute <- getCurrentRoute

        -- Get the breadcrumbs, as defined in the YesodBreadcrumbs instance.
        (title, parents) <- breadcrumbs

        -- Define the menu items of the header.
        let menuItems =
                [ NavbarLeft MenuItem
                    { menuItemLabel = "Profile"
                    , menuItemRoute = case mProfile of
                        Nothing -> HomeR
                        Just (Entity profileId _) -> ProfileR profileId ProfileDetailR
                    , menuItemAccessCallback = isJust mProfile
                    }
                , NavbarLeft MenuItem
                    { menuItemLabel = "Tables"
                    , menuItemRoute = TablesR TableListR
                    , menuItemAccessCallback = isJust muser
                    }
                , NavbarLeft MenuItem
                    { menuItemLabel = "Team"
                    , menuItemRoute = case mTeamId of
                        Nothing -> HomeR
                        Just teamId -> TeamR teamId TeamDetailR
                    , menuItemAccessCallback = isJust mTeamId
                    }
                , NavbarLeft MenuItem
                    { menuItemLabel = "Company"
                    , menuItemRoute = case mCompanyId of
                        Nothing -> HomeR
                        Just companyId -> CompanyR companyId CompanyDetailR
                    , menuItemAccessCallback = isJust mCompanyId
                    }
                , NavbarRight MenuItem
                    { menuItemLabel = "Login"
                    , menuItemRoute = AuthR LoginR
                    , menuItemAccessCallback = isNothing muser
                    }
                , NavbarRight MenuItem
                    { menuItemLabel = "Logout"
                    , menuItemRoute = AuthR LogoutR
                    , menuItemAccessCallback = isJust muser
                    }
                ]

        let navbarLeftMenuItems = [x | NavbarLeft x <- menuItems]
        let navbarRightMenuItems = [x | NavbarRight x <- menuItems]

        let navbarLeftFilteredMenuItems = [x | x <- navbarLeftMenuItems, menuItemAccessCallback x]
        let navbarRightFilteredMenuItems = [x | x <- navbarRightMenuItems, menuItemAccessCallback x]

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_css
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute
        :: App
        -> Maybe (Route App)
    authRoute _ = Just $ AuthR LoginR

    isAuthorized
        :: Route App  -- ^ The route the user is visiting.
        -> Bool       -- ^ Whether or not this is a "write" request.
        -> Handler AuthResult
    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized CommentR _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized (ProfileR _ ProfileDetailR) _ = return Authorized

    -- the profile route requires that the user is authenticated, so we
    -- delegate to that function
    isAuthorized (ProfileR profileId ProfileEditR) _ = userPermittedProfile profileId
    isAuthorized (TeamR _ ProfileCreateR) _ = userProfileNotExists
    isAuthorized MetadataFormR _ = isAuthenticated
    isAuthorized (TablesR TableListR) _ = isAuthenticated
    isAuthorized (TeamR _ TableCreateR) _ = isAuthenticated 
    isAuthorized (TableR tableId TableDetailR) _ = userPermittedTable tableId View
    isAuthorized (TableR tableId TableEditR) _ = userPermittedTable tableId Edit
    isAuthorized (TableR tableId TablePermissionsR) _ = userPermittedTable tableId Own
    isAuthorized (TableR tableId (ColumnR _ ColumnEditR)) _ = userPermittedTable tableId Edit
    isAuthorized (TableR tableId TableDeleteR) _ = userPermittedTable tableId Own
    isAuthorized (TableR tableId (ColumnR _ ColumnDeleteR)) _ = userPermittedTable tableId Own
    isAuthorized (TableR tableId (ColumnsR ColumnCreateR)) _ = userPermittedTable tableId Edit
    isAuthorized (TeamR teamId TeamDetailR) _ = inTeamInCompany teamId
    isAuthorized (CompanyR companyId (TeamsR TeamCreateR)) _ = inCompany companyId
    isAuthorized (CompanyR companyId (TeamsR TeamListR)) _ = inCompany
    isAuthorized (CompanyR companyId CompanyEditR) _ = isCompanyAdmin companyId
    isAuthorized CompanyCreateR _ = hasNoCompany
    isAuthorized (TeamR teamId TeamEditR) _ = inTeam teamId
    isAuthorized (TeamR teamId TeamJoinR) _ = canJoinTeam teamId
    isAuthorized (CompanyR companyId CompanyDetailR) _ = inCompany companyId

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent
        :: Text  -- ^ The file extension
        -> Text -- ^ The MIME content type
        -> LByteString -- ^ The contents of the file
        -> Handler (Maybe (Either Text (Route App, [(Text, Text)])))
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        return $
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger :: App -> IO Logger
    makeLogger = return . appLogger

-- Define breadcrumbs.
instance YesodBreadcrumbs App where
    -- Takes the route that the user is currently on, and returns a tuple
    -- of the 'Text' that you want the label to display, and a previous
    -- breadcrumb route.
    breadcrumb
        :: Route App  -- ^ The route the user is visiting currently.
        -> Handler (Text, Maybe (Route App))
    breadcrumb HomeR = return ("Home", Nothing)
    breadcrumb (AuthR _) = return ("Login", Just HomeR)
    breadcrumb  _ = return ("home", Nothing)

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner :: Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest :: App -> Route App
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest :: App -> Route App
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer :: App -> Bool
    redirectToReferer _ = True

    authenticate :: (MonadHandler m, HandlerSite m ~ App)
                 => Creds App -> m (AuthenticationResult App)
    authenticate creds = liftHandler $ runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Authenticated uid
            Nothing -> Authenticated <$> insert User
                { userIdent = credsIdent creds
                , userPassword = Nothing
                }

    -- You can add other plugins like Google Email, email or OAuth here
    authPlugins :: App -> [AuthPlugin App]
    authPlugins _ = [oauth2GoogleScoped ["email", "profile"] clientId clientSecret]

-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
    muid <- maybeAuthId
    return $ case muid of
        Nothing -> Unauthorized "You must login to access this page"
        Just _ -> Authorized

userPermittedTable :: TableId -> PermissionType -> Handler AuthResult
userPermittedTable tableId permissionType = do
    uid <- requireAuthId
    mProfile <- runDB $ getBy $ UniqueProfile uid
    case mProfile of
        Nothing -> return $ Unauthorized $ T.pack "You need to have a profile first."
        Just (Entity profileId _) -> do
            mPermissionRow <- runDB $ getBy $ UniquePair profileId tableId
            return $ case mPermissionRow of
                Nothing -> Unauthorized $ T.pack $ "You do not have the correct permissions to " ++ show permissionType ++ " this table."
                Just (Entity _ permissionRow) ->
                    if permissionPermissionType permissionRow >= permissionType 
                    then Authorized
                    else Unauthorized $ T.pack $ "You do not have the correct permissions to " ++ show permissionType ++ " this table."


userProfileNotExists :: Handler AuthResult
userProfileNotExists = do
    userId <- requireAuthId
    mprofile <- runDB $ getBy $ UniqueProfile userId
    return $ case mprofile of
        Nothing -> Authorized
        Just (Entity _ profile) -> Unauthorized $ T.pack "Profile for " ++ profileName profile ++ " already exists."

userPermittedProfile :: ProfileId -> Handler AuthResult
userPermittedProfile profileId = do
    userId <- requireAuthId
    profile <- runDB $ get404 profileId
    return $ if userId == profileUserId profile
               then Authorized
               else Unauthorized ("" :: Text)

canJoinTeam :: TeamId -> Handler AuthResult
canJoinTeam teamId = do
    uid <- requireAuthId
    mprofile <- runDB $ getBy $ UniqueProfile uid
    return $ case mprofile of
        Nothing -> Authorized
        Just (Entity _ profile) -> if profileTeamId profile == teamId
                          then Unauthorized ("Already joined" :: Text)
                          else Unauthorized ("Part of a different team" :: Text)

isCompanyAdmin :: CompanyId -> Handler AuthResult
isCompanyAdmin companyId = do
    uid <- requireAuthId
    company <- runDB $ get404 companyId
    return $ if companyAdminUserId company == uid
               then Authorized
               else Unauthorized ("" :: Text)

hasNoCompany :: Handler AuthResult
hasNoCompany = do
    uid <- requireAuthId
    mcompany <- runDB $ getBy $ UniqueAdmin uid
    return $ case mcompany of
        Nothing -> Authorized
        Just _ -> Unauthorized ("" :: Text)

inCompany :: CompanyId -> Handler AuthResult
inCompany companyId = do
    uid <- requireAuthId
    mProfile <- runDB $ getBy $ UniqueProfile uid
    case mProfile of
        Nothing -> isCompanyAdmin companyId
        Just (Entity _ profile) -> do
            team <- runDB $ get404 $ profileTeamId profile
            return $ if teamCompanyId team == companyId 
              then Authorized
              else Unauthorized ("" :: Text)



instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage :: App -> [Lang] -> FormMessage -> Text
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager :: App -> Manager
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
