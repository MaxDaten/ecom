{-# LANGUAGE TupleSections, OverloadedStrings, RecordWildCards #-}
module Ecom.Handler.Admin where

import Data.Colour (darken)

import Ecom.Import
import Ecom.Utils


getAdminAllUsersR :: Handler RepHtml
getAdminAllUsersR = do
        allUsers <- acidQuery AllUsers
        defaultLayout $ do
            setTitle "Admin Users"
            $(widgetFile "admin-users")
            $(widgetFile "table")


postAdminAllUsersR :: Handler RepHtml
postAdminAllUsersR = do
    ((result, widget), enctype) <- runFormPost userForm
    case result of
        FormSuccess user -> do
            mUser <- acidQuery (UserByName (username user))
            case mUser of
                Nothing -> do
                    acidUpdate (InsertUser user)
                    defaultLayout [whamlet|
                            <p>created: #{show $ username user}
                            <a .btn href=@{AdminAllUsersR}>_{MsgAdminUsers}
                          |]
                _ -> defaultLayout [whamlet|
                            <p>_{MsgUserAlreadyExisting}: <a href=@{AdminUserR (username user)}>#{username user}
                            <a .btn href=@{AdminAllUsersR}>_{MsgAdminUsers}
                           |]
        _ -> defaultLayout [whamlet|
                                <p>Falsche Eingabe
                                ^{basicUserForm widget enctype}
                            |]



getAdminCreateUserR :: Handler RepHtml
getAdminCreateUserR = do
    (widget, enctype) <- generateFormPost userForm
    defaultLayout [whamlet|
    ^{basicUserForm widget enctype}
    <a .btn href=@{AdminAllUsersR}>« _{MsgBack}|]


getAdminDeleteUserR :: Text -> Handler RepHtml
getAdminDeleteUserR name = do
    acidUpdate (DeleteUserByName name)
    getAdminAllUsersR


getAdminUserR :: Text -> Handler RepHtml
getAdminUserR = getUserR


getUserR :: Text -> Handler RepHtml
getUserR name = do
    mU <- acidQuery (UserByName name)
    case mU of
        Nothing -> notFound
        (Just user) -> defaultLayout $ do
            setTitle $ toHtml $ "User: " ++ show (username user)
            $(widgetFile "user")


userAForm :: AForm Handler User
userAForm = mkUser <$> areq textField "Name" Nothing


userForm :: Html -> MForm Handler (FormResult User, Widget)
userForm = renderDivs userAForm

basicUserForm :: Widget -> Enctype -> WidgetT Ecom IO ()
basicUserForm widget enctype = toWidget $ 
    [whamlet|
    <form method=post action=@{AdminAllUsersR} enctype=#{enctype}>
        ^{widget}
        <button .btn-primary .btn>_{MsgSubmit}
   |]

---------------------------------------------------------------------------------------------------

getAdminProductsR :: Handler RepHtml
getAdminProductsR = do
    allProducts <- acidQuery (AllProducts)
    defaultLayout $ do
        setTitle "Admin Products"
        $(widgetFile "admin-products")
        $(widgetFile "table")


getAdminAssocsR :: Handler RepHtml
getAdminAssocsR = defaultLayout $ do
    setTitle "Admin Assocs"
    $(widgetFile "admin-assocs")
