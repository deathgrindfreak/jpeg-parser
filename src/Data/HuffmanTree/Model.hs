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
  , flattenTree
  )
where

import Data.Bits (shiftL)
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as LBS
import GHC.Stack (HasCallStack)
import Data.List (intercalate)
import Text.Printf

import Data.CodeWord

data DecodeBuffer = DecodeBuffer (Maybe CodeWord) LBS.ByteString

mkDecodeBuffer :: LBS.ByteString -> DecodeBuffer
mkDecodeBuffer = DecodeBuffer Nothing

newtype Decoder a = Decoder
  { runDecoder :: DecodeBuffer -> Either String (a, DecodeBuffer)
  }

evalDecoder :: Decoder a -> DecodeBuffer -> Either String a
evalDecoder d = fmap fst . runDecoder d

getBuffer :: Decoder DecodeBuffer
getBuffer = Decoder $ \d -> Right (d, d)

putBuffer :: DecodeBuffer -> Decoder ()
putBuffer df = Decoder $ \_ -> Right ((), df)

instance Functor Decoder where
  fmap f (Decoder dc) = Decoder $ (first f <$>) . dc

instance Applicative Decoder where
  pure a = Decoder $ \d -> Right (a, d)

  (Decoder fds) <*> (Decoder dc) =
    Decoder $ \d -> do
      (f, d') <- fds d
      (a, d'') <- dc d'
      return (f a, d'')

instance Monad Decoder where
  (Decoder dc) >>= f =
    Decoder $ \d -> do
      (a, d') <- dc d
      runDecoder (f a) d'

instance Show DecodeBuffer where
  show (DecodeBuffer cw bs) =
    let start = intercalate " " . map (printf "%02X") . LBS.unpack $ LBS.take 10 bs
     in "DecodeBuffer "
        ++ show cw ++ " "
        ++ start
        ++ " (" ++ show (LBS.length bs) ++ " bytes remaining)"

data HTree a = Nil | Symbol a | Tree (HTree a) (HTree a)
  deriving (Eq, Ord, Functor, Foldable, Traversable)

instance Show a => Show (HTree a) where
  show =
    unlines
      . map (\(f, s) -> (printf "%03s" (show s)) ++ " " ++ show f)
      . flattenTree

flattenTree :: HTree a -> [(CodeWord, a)]
flattenTree = foldMap (:[]) . go (mkCodeWordFromBits (0 :: Int))
  where
    go _ Nil = Nil
    go cw (Symbol a) = Symbol (cw, a)
    go cw (Tree l r) =
      let (cl, n) = codeWordToTup cw
       in Tree
            (go (mkCodeWord (cl + 1) (n `shiftL` 1)) l)
            (go (mkCodeWord (cl + 1) (n `shiftL` 1 + 1)) r)

instance Semigroup (HTree a) where
  (<>) :: HasCallStack => HTree a -> HTree a -> HTree a
  Nil <> t = t
  t <> Nil = t
  Tree l1 r1 <> Tree l2 r2 = Tree (l1 <> l2) (r1 <> r2)
  _ <> _ = error "overlapping huffman codes"

instance Monoid (HTree a) where
  mempty = Nil
