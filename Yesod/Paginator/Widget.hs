{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Yesod.Paginator.Widget
 ( getCurrentPage
-- , getCurrentParams
 , paginationWidget
-- , paginationWidgetN
 , paginationWidgetG
 , defaultWidget
-- , defaultWidgetG
 , defaultPageWidgetConfig
 , PageWidget
 , PageWidgetG
 , PageWidgetConfig(..)
 ) where

import Yesod
import Control.Monad (when, liftM)
import Data.Maybe    (fromMaybe)
import Data.Text (Text)

import qualified Data.Text as T

-- | currentPage, itemsPerPage, totalItems -> widget
type PageWidget m = Int -> Int -> Int -> WidgetT m IO ()

data PageWidgetConfig = PageWidgetConfig
    { prevText     :: Text   -- ^ The text for the 'previous page' link.
    , nextText     :: Text   -- ^ The text for the 'next page' link.
    , pageCount    :: Int    -- ^ The number of page links to show
    , pageItems    :: Int    -- ^ The number of items per page
    , ascending    :: Bool   -- ^ Whether to list pages in ascending order.
    , showEllipsis :: Bool   -- ^ Whether to show an ellipsis if there are
                             --   more pages than pageCount
    , listClasses  :: [Text] -- ^ Additional classes for top level list
    }

-- | Individual links to pages need to follow strict (but sane) markup
--   to be styled correctly by bootstrap. This type allows construction
--   of such links in both enabled and disabled states.
data PageLink = Enabled Int Text Text -- ^ page, content, class
              | Disabled    Text Text -- ^ content, class

-- | Correctly show one of the constructed links
showLink :: [(Text, Text)] -> PageLink -> WidgetT m IO ()
showLink params (Enabled pg cnt cls) = do
    let param = ("p", showT pg)

    [whamlet|$newline never
        <li .#{cls}>
            <a href="#{updateGetParam params param}">#{cnt}
        |]

    where
        updateGetParam :: [(Text,Text)] -> (Text,Text) -> Text
        updateGetParam getParams (p, n) = (T.cons '?') . T.intercalate "&"
                                        . map (\(k,v) -> k `T.append` "=" `T.append` v)
                                        . (++ [(p, n)]) . filter ((/= p) . fst) $ getParams

showLink _ (Disabled cnt cls) =
    [whamlet|$newline never
        <li .#{cls} .disabled>
            <a>#{cnt}
        |]

-- | Default widget config provided for easy overriding of only some fields.
defaultPageWidgetConfig :: PageWidgetConfig
defaultPageWidgetConfig = PageWidgetConfig { prevText     = "«"
                                           , nextText     = "»"
                                           , pageCount    = 9
                                           , pageItems    = 10
                                           , ascending    = True
                                           , showEllipsis = True
                                           , listClasses  = ["pagination"]
                                           }

defaultWidget :: Yesod m => PageWidget m
defaultWidget = paginationWidget defaultPageWidgetConfig

-- | A widget showing pagination links. Follows bootstrap principles.
--   Utilizes a \"p\" GET param but leaves all other GET params intact.
paginationWidget :: Yesod m => PageWidgetConfig -> PageWidget m
paginationWidget (PageWidgetConfig {..}) page per tot = do
    -- total / per + 1 for any remainder
    let pages = (\(n, r) -> n + (min r 1)) $ tot `divMod` per

    when (pages > 1) $ do
        curParams <- handlerToWidget $ liftM reqGetParams getRequest

        [whamlet|$newline never
            <ul class="#{cls}">
                $forall link <- buildLinks page pages
                    ^{showLink curParams link}
            |]

    where
        -- | Concatenate all additional classes.
        cls = T.intercalate " " listClasses

        -- | Build up each component of the overall list of links. We'll
        --   use empty lists to denote ommissions along the way then
        --   concatenate.
        buildLinks :: Int -> Int -> [PageLink]
        buildLinks pg pgs =
            let prev = [1      .. pg - 1]
                next = [pg + 1 .. pgs   ]

                -- these always appear
                prevLink = [(if null prev then Disabled else Enabled (pg - 1)) prevText "prev"]
                nextLink = [(if null next then Disabled else Enabled (pg + 1)) nextText "next"]

                -- show first/last unless we're on it
                firstLink = [ Enabled 1   "1"        "prev" | pg > 1   ]
                lastLink  = [ Enabled pgs (showT pgs) "next" | pg < pgs ]

                -- we'll show ellipsis if there are enough links that some will
                -- be ommitted from the list
                prevEllipsis = [ Disabled "..." "prev" | showEllipsis && length prev > pageCount + 1 ]
                nextEllipsis = [ Disabled "..." "next" | showEllipsis && length next > pageCount + 1 ]

                -- the middle lists, strip the first/last pages and
                -- correctly take up to limit away from current
                prevLinks = reverse . take pageCount . reverse . drop 1 $ map (\p -> Enabled p (showT p) "prev") prev
                nextLinks = take pageCount . reverse . drop 1 . reverse $ map (\p -> Enabled p (showT p) "next") next

                -- finally, this page itself
                curLink = [Disabled (showT pg) "active"]

            in concat $ (if ascending then id else reverse) [ prevLink
                      , firstLink
                      , prevEllipsis
                      , prevLinks
                      , curLink
                      , nextLinks
                      , nextEllipsis
                      , lastLink
                      , nextLink
                      ]

-- | looks up the \"p\" GET param and converts it to an Int. returns a
--   default of 1 when conversion fails.
getCurrentPage :: Yesod m => HandlerT m IO Int
getCurrentPage = liftM (fromMaybe 1 . go) $ lookupGetParam "p"
    where
        go :: Maybe Text -> Maybe Int
        go mp = readIntegral . T.unpack =<< mp

showT :: (Show a) => a -> Text
showT = T.pack . show

type PageWidgetG m = Int -> Int -> Int -> WidgetT m IO ()

-- | A widget showing pagination links. Follows bootstrap principles.
--   Utilizes a \"p\" GET param but leaves all other GET params intact.
--   If there are enougth pages shows only n pages
paginationWidgetG :: Yesod m =>
                     PageWidgetConfig -- ^ Configuration
                     -> Int -- ^ Begining page to display
                     -> Int -- ^ Current page
                     -> Int -- ^ End page to display
                     -> WidgetT m IO () -- ^ The widget with page links
paginationWidgetG (PageWidgetConfig {..}) bp pg ep = do
    when (bp /= ep) $ do
        curParams <- handlerToWidget $ liftM reqGetParams getRequest
        [whamlet|$newline never
            <ul class="#{cls}">
                $forall link <- buildLinks bp pg ep
                    ^{showLink curParams link}
            |]
    where
        -- | Concatenate all additional classes.
        cls = T.intercalate " " listClasses

        -- | Build up each component of the overall list of links. We'll
        --   use empty lists to denote ommissions along the way then
        --   concatenate.
        buildLinks :: Int -> Int -> Int -> [PageLink]
        buildLinks beginPage page endPage =
            let prev = [beginPage .. page - 1]
                next = [page + 1 .. endPage]

                -- these always appear
                prevLink = [(if null prev then Disabled else Enabled (page - 1)) prevText "prev"]
                nextLink = [(if null next then Disabled else Enabled (page + 1)) nextText "next"]

                -- the middle lists, strip the first/last pages and
                -- correctly take up to limit away from current
                prevLinks = map (\p -> Enabled p (showT p) "prev") prev
                nextLinks = map (\p -> Enabled p (showT p) "next") next

                -- finally, this page itself
                curLink = [Disabled (showT page) "active"]

            in concat $ (if ascending then id else reverse) [
                      prevLink
                      , prevLinks
                      , curLink
                      , nextLinks
                      , nextLink
                      ]
