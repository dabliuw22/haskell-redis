module Domain.Products where

import Control.Exception
import Data.Typeable

newtype ProductId = ProductId { id :: String } deriving (Show, Eq)

newtype ProductName = ProductName { name :: String } deriving (Show, Eq)

newtype ProductStock = ProductStock { stock :: Double } deriving (Show, Eq)

data Product = 
  Product {
    productId :: ProductId,
    productName :: ProductName,
    productStock :: ProductStock
  } deriving (Show, Eq)

data ProductException = ProductException String deriving Show

instance Exception ProductException