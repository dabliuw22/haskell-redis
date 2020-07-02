module Domain.Products where

import Data.Text (Text)

newtype ProductId = ProductId { id :: Text } deriving (Show, Eq)

newtype ProductName = ProductName { name :: Text } deriving (Show, Eq)

newtype ProductStock = ProductStock { stock :: Double } deriving (Show, Eq)

data Product = 
  Product {
    productId :: ProductId,
    productName :: ProductName,
    productStock :: ProductStock
  } deriving (Show, Eq)