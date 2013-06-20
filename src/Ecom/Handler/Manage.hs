{-# LANGUAGE TupleSections, OverloadedStrings, RecordWildCards #-}
module Ecom.Handler.Manage where

import Data.Colour (darken)

import Ecom.Import
import Ecom.Utils


getManageUsersR :: Handler RepHtml
getManageUsersR = do
        allUsers <- acidQuery AllUsers
        defaultLayout $ do
            setTitle "Manage Users"
            $(widgetFile "manage-users")
            $(widgetFile "table")


getUserR :: Text -> Handler RepHtml
getUserR name = do
    mU <- acidQuery (UserByName name)
    case mU of
        Nothing -> notFound
        (Just user) -> defaultLayout $ do
            setTitle $ toHtml $ "User: " ++ show (username user)
            $(widgetFile "user")


getCreateUserR :: Handler RepHtml
getCreateUserR = do
    (widget, enctype) <- generateFormPost userForm
    defaultLayout [whamlet|
    ^{basicUserForm widget enctype}
    <a .btn href=@{ManageUsersR}>« _{MsgBack}|]


postCreateUserR :: Handler RepHtml
postCreateUserR = do
    ((result, widget), enctype) <- runFormPost userForm
    case result of
        FormSuccess user -> do
            mUser <- acidQuery (UserByName (username user))
            case mUser of
                Nothing -> do
                    acidUpdate (InsertUser user)
                    defaultLayout [whamlet|
                            <p>created: #{show $ username user}
                            <a .btn href=@{ManageUsersR}>_{MsgManageUsers}
                          |]
                _ -> defaultLayout [whamlet|<p>_{MsgUserAlreadyExisting}|]
        _ -> defaultLayout [whamlet|
                                <p>Falsche Eingabe
                                ^{basicUserForm widget enctype}
                            |]


userAForm :: AForm Handler User
userAForm = mkUser <$> areq textField "Name" Nothing


userForm :: Html -> MForm Handler (FormResult User, Widget)
userForm = renderDivs userAForm

basicUserForm :: Widget -> Enctype -> WidgetT Ecom IO ()
basicUserForm widget enctype = toWidget $ 
    [whamlet|
    <form method=post action=@{CreateUserR} enctype=#{enctype}>
        ^{widget}
        <button .btn-primary .btn>_{MsgSubmit}
   |]

---------------------------------------------------------------------------------------------------

getManageProductsR :: Handler RepHtml
getManageProductsR = do
    allProducts <- acidQuery (AllProducts)
    defaultLayout $ do
        setTitle "Manage Products"
        $(widgetFile "manage-products")
        $(widgetFile "table")


getManageAssocsR :: Handler RepHtml
getManageAssocsR = defaultLayout $ do
    setTitle "Manage Assocs"
    $(widgetFile "manage-assocs")
