{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module App where

import Yesod
import Yesod.Paginator
import Network.Wai.Handler.Warp (run)

data App = App

mkYesod "App" [parseRoutes|
    / RootR GET
|]

instance Yesod App where 
    approot = ApprootRelative
    defaultLayout widget = do
        pc <- widgetToPageContent widget
        withUrlRenderer [hamlet|$newline never
            $doctype 5
            <html lang="en">
                <head>
                    <meta charset="utf-8">
                    <title>#{pageTitle pc}
                    <!-- steal boostrap -->
                    <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css">
                    ^{pageHead pc}
                <body>
                    ^{pageBody pc}
            |]

getRootR :: Handler Html
getRootR = do
    -- unneeded return here to match README
    (things, widget) <-
      paginateWithG defaultPageWidgetConfig
                    paginationWidgetM
                    (listP [1..1142])

    defaultLayout $ do
        setTitle "My title"
        [whamlet|$newline never
         <h1>Pagination
            <p>The things:
            <ul>
                $forall thing <- things
                    <li>Thing #{show thing}

            <div .pagination>
                ^{widget}
            |]

main :: IO ()
main = run 3001 =<< toWaiApp App
