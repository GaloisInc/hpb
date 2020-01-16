------------------------------------------------------------------------
-- |
-- Module           : Data.HPB.Partial
-- Description      : Provides a monad that may fail with a string message.
-- Copyright        : (c) Galois, Inc 2015
-- Maintainer       : Joe Hendrix <jhendrix@galois.com>
-- Stability        : provisional
--
-- This module declare the Partial monad.
------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.HPB.Partial
  ( PartialT(..)
  , Partial
  , runPartial
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State.Class

-- | A Monad that may run or print an error.
newtype PartialT m a = PartialT { runPartialT :: m (Either String a) }

instance Functor m => Functor (PartialT m) where
  fmap f m = PartialT $ fmap (fmap f) $ runPartialT m

instance (Functor m, Monad m) => Applicative (PartialT m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (PartialT m) where
  return v = PartialT $ return $ Right v
  m >>= h = PartialT $ do
    r <- runPartialT m
    case r of
      Left e -> return $ Left e
      Right v -> runPartialT (h v)

instance Monad m => MonadFail (PartialT m) where
  fail msg = PartialT $ return $ Left msg

type Partial = PartialT Identity

runPartial :: Partial a -> Either String a
runPartial = runIdentity . runPartialT

instance MonadState s m => MonadState s (PartialT m) where
  get = PartialT $ Right `liftM` get
  put s = PartialT $ put s >> return (Right ())
