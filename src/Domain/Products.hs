module Domain.Products where

newtype ProductId = ProductId { id :: String } deriving (Show, Eq)

newtype ProductName = ProductName { name :: String } deriving (Show, Eq)

newtype ProductStock = ProductStock { stock :: Double } deriving (Show, Eq)

data Product = 
  Product {
    productId :: ProductId,
    productName :: ProductName,
    productStock :: ProductStock
  } deriving (Show, Eq)