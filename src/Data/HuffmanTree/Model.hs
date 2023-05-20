{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs #-}

module Data.HuffmanTree.Model
  ( HTree (..)
  , DecodeBuffer (..)
  , mkDecodeBuffer
  , Decoder (..)
  , evalDecoder
  , getBuffer
  , putBuffer
  )
where

import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as LBS
import GHC.Stack (HasCallStack)

import Data.HuffmanTree.CodeWord

data DecodeBuffer a = DecodeBuffer (Maybe (CodeWord a)) LBS.ByteString
  deriving (Show)

mkDecodeBuffer :: LBS.ByteString -> DecodeBuffer a
mkDecodeBuffer = DecodeBuffer Nothing

instance Functor DecodeBuffer where
  fmap f (DecodeBuffer cw bs) = DecodeBuffer ((fmap . fmap) f cw) bs

newtype Decoder s a = Decoder
  { runDecoder :: DecodeBuffer s -> Either String (a, DecodeBuffer s)
  }

evalDecoder :: Decoder s a -> DecodeBuffer s -> Either String a
evalDecoder d = fmap fst . runDecoder d

getBuffer :: Decoder s (DecodeBuffer s)
getBuffer = Decoder $ \d -> Right (d, d)

putBuffer :: DecodeBuffer s -> Decoder s ()
putBuffer df = Decoder $ \_ -> Right ((), df)

instance Functor (Decoder s) where
  fmap f (Decoder dc) = Decoder $ (first f <$>) . dc

instance Applicative (Decoder s) where
  pure a = Decoder $ \d -> Right (a, d)

  (Decoder fds) <*> (Decoder dc) =
    Decoder $ \d -> do
      (f, d') <- fds d
      (a, d'') <- dc d'
      return (f a, d'')

instance Monad (Decoder s) where
  (Decoder dc) >>= f =
    Decoder $ \d -> do
      (a, d') <- dc d
      runDecoder (f a) d'

-- instance (Show a, FiniteBits a) => Show (DecodeBuffer a) where
--   show (DecodeBuffer cw _) = "DecodeBuffer " ++ show cw ++ " <ByteString>"

data HTree a = Nil | Symbol a | Tree (HTree a) (HTree a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Semigroup (HTree a) where
  (<>) :: HasCallStack => HTree a -> HTree a -> HTree a
  Nil <> t = t
  t <> Nil = t
  Tree l1 r1 <> Tree l2 r2 = Tree (l1 <> l2) (r1 <> r2)
  _ <> _ = error "overlapping huffman codes"

instance Monoid (HTree a) where
  mempty = Nil
