{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

-------------------------------------------------------------------------------
-- |
--
-- Inspiration from a concept by ajdunlap:
--      <http://hackage.haskell.org/package/yesod-paginate>
--
-- But uses an entirely different approach.
--
-- There are two pagination functions. One for arbitrary items where you
-- provide the list of things to be paginated:
--
-- > getSomeRoute = do
-- >     things' <- getAllThings
-- >
-- >     (things, widget) <- paginate 10 things'
-- >
-- >     defaultLayout $ do
-- >         [whamlet|
-- >             $forall thing <- things
-- >                 ^{showThing thing}
-- >
-- >             <div .pagination>
-- >                  ^{widget}
-- >             |]
--
-- And another for paginating directly out of the database, you provide
-- the same filters as you would to @selectList@.
--
-- > getSomeRoute something = do
-- >     -- note: things is [Entity val] just like selectList returns
-- >     (things, widget) <- runDB $ selectPaginated 10 [SomeThing ==. something] []
-- >
-- >     defaultLayout $ do
-- >         [whamlet|
-- >             $forall thing <- things
-- >                 ^{showThing $ entityVal thing}
-- >
-- >             <div .pagination>
-- >                  ^{widget}
-- >             |]
--
-- Both functions return a tuple: the first element being the list of
-- items (or Entities) to display on this page and the second being a
-- widget showing the pagination navagation links.
--
-------------------------------------------------------------------------------
module Yesod.Paginator
    ( paginate
    , paginateWith
    , selectPaginated
    , paginateWithG
    , paginateG
    , listP
    , selectListP
    , rawSqlP
    , runDBP
    , module Yesod.Paginator.Widget
    ) where

import Yesod
import Yesod.Paginator.Widget
import Control.Monad.Trans.Reader
import Database.Persist.Sql
import qualified Data.Text as T

-- |
paginate :: Yesod m => Int -> [a] -> HandlerT m IO ([a], WidgetT m IO ())
paginate = paginateWith defaultWidget

paginateWith :: Yesod m
             => PageWidget m
             -> Int
             -> [a]
             -> HandlerT m IO ([a], WidgetT m IO ())
paginateWith widget per items = do
    p <- getCurrentPage

    let tot = length items
    let  xs = take per $ drop ((p - 1) * per) items

    return (xs, widget p per tot)

selectPaginated :: forall m val.
                   (PersistEntity val
                   , PersistQuery (PersistEntityBackend val)
                   , Yesod m) =>
                   Int
                   -> [Filter val]
                   -> [SelectOpt val]
                   -> ReaderT
                   (PersistEntityBackend val) (HandlerT m IO) ([Entity val], WidgetT m IO ())
selectPaginated = selectPaginatedWith defaultWidget

selectPaginatedWith :: forall m val t.
                       (PersistEntity val
                       , PersistQuery (PersistEntityBackend val)
                       , Yesod m) =>
                       (Int -> Int -> Int -> t)
                       -> Int
                       -> [Filter val]
                       -> [SelectOpt val]
                       -> ReaderT
                       (PersistEntityBackend val) (HandlerT m IO) ([Entity val], t)
selectPaginatedWith widget per filters selectOpts = do
    p   <- lift getCurrentPage
    tot <- count filters
    xs  <- selectList filters (selectOpts ++ [OffsetBy ((p-1)*per), LimitTo per])
    return (xs, widget p per tot)

-- | Paginate with default config 
paginateG :: forall m a.
                 Yesod m =>
                 (Int -> Int -> HandlerT m IO [a])
                 -> HandlerT m IO ([a], WidgetT m IO ())
paginateG = paginateWithG defaultPageWidgetConfig paginationWidgetG

listP :: forall (m :: * -> *) a.
               Monad m =>
               [a] -> Int -> Int -> m [a]
listP xs offset limit = return . take limit . drop offset $ xs

selectListP :: forall val (m :: * -> *).
                     (PersistEntity val, PersistQuery (PersistEntityBackend val),
                      MonadIO m) =>
                     [Filter val]
                     -> [SelectOpt val]
                     -> Int
                     -> Int
                     -> ReaderT (PersistEntityBackend val) m [Entity val]
selectListP flt opt offset limit = selectList flt (opt ++ [OffsetBy offset,LimitTo limit])

rawSqlP :: forall (m :: * -> *) a a1.
                 (RawSql a, PersistField a1, MonadIO m) =>
                 T.Text -> [PersistValue] -> a1 -> a1 -> ReaderT SqlBackend m [a]
rawSqlP query opt offset limit = rawSql (query `T.append` " OFFSET ? LIMIT ? ") (opt ++ map toPersistValue [offset,limit])

runDBP :: forall site a t t1.
                YesodPersist site =>
                (t -> t1 -> YesodDB site a) -> t -> t1 -> HandlerT site IO a
runDBP f limit offset = runDB $ f limit offset

paginateWithG :: forall m a t.
                 Yesod m =>
                 PageWidgetConfig
                 -> (PageWidgetConfig -> Int -> Int -> Int -> t)
                 -> (Int -> Int -> HandlerT m IO [a])
                 -> HandlerT m IO ([a], t)
paginateWithG pc widget f = do
    page <- getCurrentPage
    (xs,beginPage,page',endPage) <- obtainBounds pc page f
    return (xs, widget pc beginPage page' endPage)

obtainBounds :: forall (m :: * -> *) a.
                    Monad m =>
                    PageWidgetConfig
                    -> Int -> (Int -> Int -> m [a]) -> m ([a], Int, Int, Int)
obtainBounds (PageWidgetConfig {..}) page f =
  obtainPage' bp0 page ep0
 where bp0 = (max (page - pageCount `div` 2) 1)
       ep0 = bp0 + (pageCount - 1)
       obtainPage' bp p ep = do
         let offset = (p - 1)*pageItems
             limit  = (ep - p + 1)*pageItems
         toEnd <- f offset limit
         let n = length toEnd
         let xs = take pageItems toEnd
             (a,b) = n `divMod` pageItems
             ep'   = p + a - if b == 0 then 1 else 0
         return (xs,bp,p,ep')

