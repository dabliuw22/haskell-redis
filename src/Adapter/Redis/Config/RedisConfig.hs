module Adapter.Redis.Config.RedisConfig (Configuration(..), create) where

import Database.Redis

data Configuration =
  Configuration {
    host :: String,
    port :: Integer,
    maxConnections :: Int
  }

connectInfo :: Configuration ->  ConnectInfo
connectInfo conf = defaultConnectInfo {
  connectHost = host conf,
  connectPort = PortNumber $ fromIntegral (port conf),
  connectMaxConnections = maxConnections conf
}

create :: Configuration -> IO Connection
create conf = connect (connectInfo conf)