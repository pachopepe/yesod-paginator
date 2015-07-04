{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE RankNTypes        #-}
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
    , selectPaginatedWith
    , rawSqlPaginated
    , rawSqlPaginatedWith
    , module Yesod.Paginator.Widget
    ) where

import Yesod
import Yesod.Paginator.Widget
import Control.Monad.Trans.Reader
import qualified Data.Text as T
import Database.Persist.Sql
import qualified Data.Conduit.List as CL
import Data.ByteString.Char8 (readInteger)

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

rawSqlPaginated :: (Yesod m, RawSql a) =>
                   Int
                   -> T.Text
                   -> T.Text
                   -> T.Text
                   -> T.Text
                   -> [PersistValue]
                   -> ReaderT SqlBackend (HandlerT m IO) ([a], WidgetT m IO ())
rawSqlPaginated = rawSqlPaginatedWith defaultWidget


rawSqlPaginatedWith :: forall m a.
                             (Yesod m, RawSql a) =>
                             PageWidget m
                             -> Int
                             -> T.Text
                             -> T.Text
                             -> T.Text
                             -> T.Text
                             -> [PersistValue]
                             -> ReaderT SqlBackend (HandlerT m IO) ([a], WidgetT m IO ())
rawSqlPaginatedWith widget per select from wher order opts = do
    p   <- lift getCurrentPage
    tot <- getCount (T.concat [ "SELECT count(*) "
                              , " FROM " `cndAppend` from
                              , " WHERE " `cndAppend` wher]) opts
    xs  <- rawSql (T.concat ["SELECT " , select
                            , " FROM " `cndAppend` from
                            , " WHERE " `cndAppend` wher
                            , " ORDER BY " `cndAppend` order
                            ," LIMIT "
                            ,T.pack . show $ per
                            ," OFFSET "
                            ,T.pack . show $ ((p-1)*per)])
                  opts
    return (xs, widget p per $ fromIntegral tot)
  where cndAppend :: T.Text -> T.Text -> T.Text
        cndAppend prefix t = if T.null t then T.empty else prefix `T.append` t 

getCount :: forall a (m :: * -> *).
            (MonadIO m, Num a) =>
            T.Text -> [PersistValue] -> ReaderT SqlBackend m a
getCount sql opts = withRawQuery sql opts $ do
  mm <- CL.head
  case mm of
   Just [PersistInt64 i] -> return $ fromIntegral i
   Just [PersistDouble i] ->return $ fromIntegral (truncate i) -- gb oracle
   Just [PersistByteString i] -> case readInteger i of -- gb mssql 
                                  Just (ret,"") -> return $ fromIntegral ret
                                  xs -> error $ "invalid number i["++show i++"] xs[" ++ show xs ++ "]"
   Just xs -> error $ "count:invalid sql  return xs["++show xs++"] sql["++show sql++"]"
   Nothing -> error $ "count:invalid sql returned nothing sql["++show sql++"]"
