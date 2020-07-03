{-# LANGUAGE OverloadedStrings #-}
module Adapter.Redis.Products (ProductRepository(..)) where

import Adapter.Redis.Config.RedisConfig
import qualified Database.Redis as R
import qualified Data.ByteString.Char8 as B
import qualified Domain.Products as P
import Data.Maybe (fromJust)
import Control.Monad.Catch
import Control.Monad.IO.Class

fields = ["id", "name", "stock"]

class (Functor m, Monad m, MonadCatch m) => ProductRepository m where
  get :: R.Connection -> String -> m (Maybe P.Product)
  set :: R.Connection -> P.Product -> m ()
  del :: R.Connection -> String -> m ()

instance ProductRepository IO where
  get conn id = do
    result <- exec conn $ R.hmget (B.pack id) fields
    case result of
          Right list -> case list of
                              (h : t) -> toDomain (h:t)
                              []      -> return Nothing
          Left e     -> throwM $ P.ProductException ("Not Found: " ++ show id)
  set conn product = do
    cmd <- fromDomain product
    let key = B.pack $ P.id (P.productId product)
    status <- exec conn $ R.hmset key cmd
    case status of
          Right R.Ok -> expire conn key >>= \s -> return ()
          Left e     -> throwM $ P.ProductException "Not Create"
  del conn id = do
    result <- exec conn $ R.hdel (B.pack id) fields
    case result of
          Right a -> return ()
          Left e  -> throwM $ P.ProductException "Not Delete"

expire :: MonadIO m => R.Connection -> B.ByteString -> m Bool
expire conn key = do
  result <- exec conn $ R.expire key expSeconds
  case result of
        Right b -> return b
        _       -> return False
  where
    expSeconds :: Integer
    expSeconds = 60

exec :: MonadIO m => R.Connection -> R.Redis a -> m a
exec conn action = do
  liftIO $ R.runRedis conn action

toDomain :: Monad m => [Maybe B.ByteString] -> m (Maybe P.Product)
toDomain list = do
  case list of
    [Nothing, Nothing, Nothing]   -> return Nothing
    fields @ [id', name', stock'] -> let newList = map (B.unpack . fromJust) fields in
      return $ Just $ P.Product {
        P.productId = P.ProductId { P.id = head newList },
        P.productName = P.ProductName { P.name = newList !! 1 },
        P.productStock = P.ProductStock { P.stock = read (newList !! 2) :: Double }
      }
    _                             -> return Nothing

fromDomain :: Monad m => P.Product -> m [(B.ByteString, B.ByteString)]
fromDomain product = do
  return [
          (B.pack "id", B.pack (P.id (P.productId product))),
          (B.pack "name", B.pack (P.name (P.productName product))),
          (B.pack "stock", B.pack (show(P.stock (P.productStock product))))
         ]