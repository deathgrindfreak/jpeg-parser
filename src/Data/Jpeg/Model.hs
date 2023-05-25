{-# LANGUAGE DeriveFunctor #-}

module Data.Jpeg.Model
  ( JpegData (..)
  , JpegParseError (..)
  , QuantizationTable (..)
  , QuantizationType (..)
  , Block (..)
  , DecodeBlock
  , DCTBlock
  , BlockComponent (..)
  , DecodeBlockComponent
  , DCTBlockComponent
  , ComponentType (..)
  , Component (..)
  , StartOfFrame (..)
  , TreeType (..)
  , HuffmanTree (..)
  , ScanData
  , Jpeg (..)
  , getQType
  )
where

import Control.Exception (Exception (..))
import Data.Matrix (Matrix)
import Data.Vector (Vector)
import Data.Word (Word8)

import Data.HuffmanTree

data Jpeg = Jpeg
  { headerData :: JpegData
  , scanData :: ScanData
  }
  deriving (Show)

type ScanData = [DCTBlock]

data JpegData = JpegData
  { quantizationTables :: [QuantizationTable]
  , startOfFrame :: StartOfFrame
  , huffmanTrees :: [HuffmanTree]
  }
  deriving (Eq, Show)

newtype JpegParseError = JpegParseError String
  deriving (Show)

instance Exception JpegParseError

data QuantizationType = Luminance | Chrominance
  deriving (Eq, Ord, Enum, Bounded, Show)

data QuantizationTable = QuantizationTable
  { quantizationType :: QuantizationType
  , quantizationTable :: Vector Int
  }
  deriving (Eq, Show)

data ComponentType = Y | Cb | Cr | I | Q
  deriving (Eq, Ord, Enum, Bounded, Show)

getQType :: ComponentType -> QuantizationType
getQType cType =
  case cType of
    Y -> Luminance
    _ -> Chrominance

data Component = Component
  { componentType :: ComponentType
  , samplingFactors :: (Int, Int)
  , quantizationTableNumber :: Int
  }
  deriving (Eq, Show)

type DecodeBlock = Block (Vector Int)
type DCTBlock = Block (Matrix Int)

newtype Block a = Block {fromBlock :: [BlockComponent a]}
  deriving (Eq, Show, Functor)

type DecodeBlockComponent = BlockComponent (Vector Int)
type DCTBlockComponent = BlockComponent (Matrix Int)

data BlockComponent value = BlockComponent
  { blockComponentType :: ComponentType
  , blockValues :: value
  , blockQuantizationTableNumber :: Int
  }
  deriving (Eq, Show, Functor)

data StartOfFrame = StartOfFrame
  { precision :: Int
  , height :: Int
  , width :: Int
  , components :: [Component]
  }
  deriving (Eq, Show)

data TreeType = DC | AC deriving (Eq, Ord, Enum, Bounded, Show)

data HuffmanTree = HuffmanTree
  { quantType :: QuantizationType
  , treeType :: TreeType
  , tree :: HTree Word8
  }
  deriving (Eq, Show)
