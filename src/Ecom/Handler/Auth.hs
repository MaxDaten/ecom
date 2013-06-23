module Ecom.Handler.Auth where

import Ecom.Import


getLoginUserR :: Text -> Handler RepHtml
getLoginUserR username = do
	setSession "name" username
	setInfoMessageI (MsgLoginAs username)
	redirect HomeR

getLogoutR :: Handler RepHtml
getLogoutR = clearSession >> setInfoMessageI MsgLoggedOut >> redirect HomeR