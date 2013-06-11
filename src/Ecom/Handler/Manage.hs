module Ecom.Handler.Manage where

import Ecom.Import


getManageUsersR :: Handler RepHtml
getManageUsersR = defaultLayout $ do
    setTitle "Manage Users"
    $(widgetFile "manage-users")


getManageProductsR :: Handler RepHtml
getManageProductsR = defaultLayout $ do
    setTitle "Manage Products"
    $(widgetFile "manage-products")


getManageAssocsR :: Handler RepHtml
getManageAssocsR = defaultLayout $ do
    setTitle "Manage Assocs"
    $(widgetFile "manage-assocs")
