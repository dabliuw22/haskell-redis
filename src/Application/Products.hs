module Application.Products (ProductService(..)) where

import qualified Domain.Products as P

class (Functor m, Monad m) => ProductService m where
  get' :: (String -> m (Maybe P.Product)) -> String -> m (Maybe P.Product)
  set' :: (P.Product -> m ()) -> P.Product -> m ()
  del' :: (String -> m ()) -> String -> m ()
  
instance ProductService IO where
  get' f id = f id
  set' f product = f product
  del' f id = f id