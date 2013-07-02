{-# LANGUAGE TupleSections, OverloadedStrings, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-warnings-deprecations -fno-warn-name-shadowing -fno-warn-hi-shadowing #-}
module Ecom.Handler.Admin where

import             Ecom.Import
import             Ecom.Utils
import             Ecom.Forms

import             Data.List (intersperse)
import             Data.Set (toList)
import             Control.Arrow ((&&&))


getAdminAllUsersR :: Handler RepHtml
getAdminAllUsersR = do
        allUsers <- acidQuery AllUsers
        defaultLayout $ do
            setTitle "Admin Users"
            $(widgetFile "admin/users")
            $(widgetFile "table")


postAdminAllUsersR :: Handler RepHtml
postAdminAllUsersR = do
    ((result, _), _) <- runFormPost userForm
    case result of
        FormSuccess user -> do
            mUser <- acidQuery (UserByName (username user))
            case mUser of
                Nothing -> do
                    acidUpdate (InsertUser user)
                    setInfoMessageI $ MsgUserCreated (username user)
                _ -> setErrorMessageI MsgUserAlreadyExisting
        _ -> setErrorMessageI MsgInvalidInput
    redirect AdminAllUsersR


postAdminClearHistoryR :: Text -> Handler RepHtml
postAdminClearHistoryR username = do
    mUser <- acidQuery (UserByName username)
    case mUser of
        Nothing -> setErrorMessageI MsgInvalidUser
        Just user -> do
            acidUpdate (ClearUserHistory user)
            setInfoMessageI MsgUserHistoryCleared
    redirect AdminAllUsersR
            

postAdminDeleteHistoryEntry :: Text -> Int -> Handler RepHtml
postAdminDeleteHistoryEntry username i = do
    mUser <- acidQuery (UserByName username)
    case mUser of
        Nothing -> do 
            setErrorMessageI MsgInvalidUser
            redirect AdminAllUsersR
        Just user -> do
            acidUpdate (DeleteHistoryEntry user i)
            setInfoMessageI MsgUserHistoryEntryDeleted
            redirect (AdminUserR username)


getAdminCreateUserR :: Handler RepHtml
getAdminCreateUserR = do
    (widget, enctype) <- generateFormPost userForm
    defaultLayout [whamlet|
    ^{basicUserForm widget enctype}
    <a .btn href=@{AdminAllUsersR}>« _{MsgBack}|]

---------------------------------------------------------------------------------------------------

getAdminDeleteUserR :: Text -> Handler RepHtml
getAdminDeleteUserR name = do
    acidUpdate (DeleteUserByName name)
    redirect AdminAllUsersR


getAdminUserR :: Text -> Handler RepHtml
getAdminUserR = getUserR


getUserR :: Text -> Handler RepHtml
getUserR name = do
    mU <- acidQuery (UserByName name)
    case mU of
        Nothing -> notFound
        Just user -> defaultLayout $ do
            setTitle $ toHtml $ "User: " ++ show (username user)
            $(widgetFile "user")
    where idxList = zip ([0..])


getAdminEditUserR :: Text -> Handler RepHtml
getAdminEditUserR name = do
    mU <- acidQuery (UserByName name)
    case mU of
        Nothing -> notFound
        Just user -> do
            (widget, enctype) <- generateFormPost $ renderTable $ attributesAFormWithDefault (attributes user)
            defaultLayout [whamlet|
                <form method=post action=@{AdminEditUserR name} enctype=#{enctype}>
                    ^{widget}
                    <button .btn-primary .btn>_{MsgSubmit}
            |]


postAdminEditUserR :: Text -> Handler RepHtml
postAdminEditUserR name = do
    mU <- acidQuery (UserByName name)
    case mU of
        Nothing -> notFound
        Just user -> do
            ((result, _), _ ) <- runFormPost $ renderTable attributesAForm
            case result of
                FormSuccess attrs -> do
                    _ <- acidUpdate (SetUserAttributes user attrs)
                    setInfoMessageI MsgUserUpdated
                _ -> setErrorMessageI MsgInvalidInput
    redirect (UserR name)
---------------------------------------------------------------------------------------------------

getAdminCreateProductR :: Handler RepHtml
getAdminCreateProductR = do
    (widget, enctype) <- generateFormPost productForm
    defaultLayout [whamlet|
    ^{basicProductForm widget enctype}
    <a .btn href=@{AdminAllUsersR}>« _{MsgBack}|]


postAdminCreateProductR :: Handler RepHtml
postAdminCreateProductR = do
    ((result, _), _) <- runFormPost productForm
    case result of
        FormSuccess product -> do
            acidUpdate (InsertProduct product)
            setInfoMessageI MsgProductAdded
        _ -> setErrorMessageI MsgInvalidInput
    redirect AdminAllProductsR


getAdminAllProductsR :: Handler RepHtml
getAdminAllProductsR = do
    allProducts <- acidQuery (AllProducts)
    defaultLayout $ do
        setTitle "Admin Products"
        $(widgetFile "admin/products")
        $(widgetFile "table")


---------------------------------------------------------------------------------------------------

getAdminCreateAssocR :: Handler RepHtml
getAdminCreateAssocR = do
    (widget, enctype) <- generateFormPost assocForm
    defaultLayout [whamlet|
    ^{basicAssocForm widget enctype}
    <a .btn href=@{AdminAssocsR}>« _{MsgBack}
    |]

postAdminCreateAssocR :: Handler RepHtml
postAdminCreateAssocR = do
    ((result, _), _) <- runFormPost assocForm
    case result of
        FormSuccess assoc -> do
            acidUpdate (InsertAssoc assoc)
            setInfoMessageI MsgAssocCreated
        _ -> setErrorMessageI MsgInvalidInput
    redirect AdminAssocsR


getAdminAssocsR :: Handler RepHtml
getAdminAssocsR = do
    allA <- acidQuery (AllAssocs)
    let allAssocs = map toText allA
    defaultLayout $ do
        setTitle "Admin Assocs"
        $(widgetFile "admin/assocs")
    where
        toText :: Association -> (Text, [Text])
        toText = (unProductCategory . assocCategory) &&& ((map unProductCategory) . toList . assocedCategories)

getAdminDeleteAssocR :: Text -> Handler RepHtml
getAdminDeleteAssocR name = do
    acidUpdate (DeleteAssocByName name)
    redirect AdminAssocsR
