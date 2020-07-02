{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Adapter.Redis.Config.RedisConfig as R
import Adapter.Redis.Products
import Control.Monad.IO.Class
import Data.Maybe (fromJust, fromMaybe)
import qualified Domain.Products as P
import Data.UUID.V1 (nextUUID)
import qualified Data.UUID as UUID
import System.Environment

main :: IO ()
main = do
  conf <- load
  conn <- R.create conf
  newProduct <- new
  insert <- set conn newProduct
  one <- get conn (P.id (P.productId newProduct))
  print one

load :: MonadIO m => m R.Configuration
load = do
  host' <- liftIO $ lookupEnv "REDIS_HOST"
  port' <- liftIO $ lookupEnv "REDIS_PORT"
  maxConn' <- liftIO $ lookupEnv "REDIS_MAX_CONN"
  return R.Configuration {
           R.host = fromMaybe "localhost" host',
           R.port = read (fromMaybe "6379" port') :: Integer,
           R.maxConnections = read (fromMaybe "100" maxConn') :: Int
         }

new :: MonadIO m => m P.Product
new = do
  uuid <- liftIO $ nextUUID
  let uuid' = UUID.toString (fromJust uuid)
  return P.Product {
    P.productId = P.ProductId { P.id = uuid' },
    P.productName = P.ProductName { P.name = uuid' },
    P.productStock = P.ProductStock { P.stock = 100.0 }
  }
