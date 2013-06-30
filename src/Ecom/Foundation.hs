module Ecom.Foundation where

import              Prelude
import              Control.Applicative
import              Data.Maybe                  (isJust)
----------------------------------------------------------------------------------------------------
import              Yesod
import              Yesod.Static
import              Yesod.Default.Config
import              Yesod.Default.Util          (addStaticContentExternal)
----------------------------------------------------------------------------------------------------
import              Network.HTTP.Conduit        (Manager)
import              Text.Jasmine                (minifym)
import              Text.Hamlet                 (hamletFile)
import              Data.Text                   (Text)
import qualified    Data.Text                   as T
import qualified    Data.Text.Lazy              as LT
import qualified    Text.Blaze.Html.Renderer.Text as RenderText
import              System.Log.FastLogger       (Logger)
----------------------------------------------------------------------------------------------------
import              Data.Acid
import              Data.Acid.Advanced
----------------------------------------------------------------------------------------------------
import qualified    Ecom.Settings               as Settings
import              Ecom.Settings.Development   (development)
import              Ecom.Settings.StaticFiles
import              Ecom.Settings               (widgetFile, Extra (..))
import              Ecom.Model
----------------------------------------------------------------------------------------------------

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data Ecom = Ecom
    { settings      :: AppConfig DefaultEnv Extra
    , getStatic     :: Static -- ^ Settings for static file serving.
    , httpManager   :: Manager
    , getEcomState  :: AcidState EcomState
    , appLogger     :: Logger
    }

-- Set up i18n messages. See the message folder.
mkMessage "Ecom" "messages" "de"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype AppRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route App = AppRoute
-- * Creates the value resourcesApp which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- App. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the AppRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "Ecom" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT Ecom IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod Ecom where
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        (120 * 60) -- 120 minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage
        imsg <- getInfoMessage
        emsg <- getErrorMessage
        loggedInUser <- lookupSession "name"

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.
        header <- widgetToPageContent $ do
            addScript $ StaticR js_jquery_1_10_1_min_js
            --addScript $ StaticR js_angular_1_1_5_min_js -- unstable for anims
            addScript $ StaticR js_angular_min_js -- stable
            addScript $ StaticR js_jquery_ui_1_10_3_js
            addScript $ StaticR js_bootstrap_min_js
            $(widgetFile "header")

        pc <- widgetToPageContent $ do
            $(combineStylesheets 'StaticR
                [ css_normalize_css
                , css_bootstrap_min_css
                , css_font_awesome_min_css
                , css_jquery_ui_1_10_0_css
                ])
            $(widgetFile "default-layout")
        hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent =
        addStaticContentExternal minifym genFileName Settings.staticDir (StaticR . flip StaticRoute [])
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs
            | development = "autogen-" ++ base64md5 lbs
            | otherwise   = base64md5 lbs

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

    makeLogger = return . appLogger

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage Ecom FormMessage where
    renderMessage _ _ = defaultFormMessage

isLoggedIn :: Handler Bool
isLoggedIn = do
    mUser <- lookupSession "name"
    return . isJust $ mUser

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod

----------------------------------------------------------------------------------------------------

infoMessageId :: Text
infoMessageId = "_MSIMSG"

errorMessageId :: Text
errorMessageId = "_MSEMSG"

setMessageHtml:: (MonadHandler m) => Text -> Html -> m ()
setMessageHtml msgId = 
    setSession msgId . T.concat . LT.toChunks . RenderText.renderHtml

getMessageHtml :: (MonadHandler m) => Text -> m (Maybe Html)
getMessageHtml msgId = do
    mMsg <- fmap (fmap preEscapedToMarkup) $ lookupSession msgId
    deleteSession msgId
    return mMsg

setErrorMessage :: (MonadHandler m) => Html -> m ()
setErrorMessage = setMessageHtml errorMessageId

setErrorMessageI :: (MonadHandler m, RenderMessage (HandlerSite m) msg)
                 => msg -> m ()
setErrorMessageI msg = do
    mr <- getMessageRender
    setErrorMessage (toHtml (mr msg))

getErrorMessage :: MonadHandler m => m (Maybe Html)
getErrorMessage = getMessageHtml errorMessageId

setInfoMessage :: (MonadHandler m) => Html -> m ()
setInfoMessage = setMessageHtml infoMessageId

setInfoMessageI :: (MonadHandler m, RenderMessage (HandlerSite m) msg)
                 => msg -> m ()
setInfoMessageI msg = do
    mr <- getMessageRender
    setInfoMessage (toHtml (mr msg))

getInfoMessage :: MonadHandler m => m (Maybe Html)
getInfoMessage = getMessageHtml infoMessageId

----------------------------------------------------------------------------------------------------

acidQuery :: (QueryEvent event, MethodState event ~ EcomState) => event -> Handler (EventResult event)
acidQuery q = do
  state <- getEcomState <$> getYesod
  query' state q

acidUpdate :: (UpdateEvent event, MethodState event ~ EcomState) => event -> Handler (EventResult event)
acidUpdate q = do
  state <- getEcomState <$> getYesod
  update' state q
