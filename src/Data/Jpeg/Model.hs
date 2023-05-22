module Data.Jpeg.Model
  ( JpegData (..)
  , JpegParseError (..)
  , QuantizationTable (..)
  , QuantizationType (..)
  , Block (..)
  , BlockComponent (..)
  , ComponentType (..)
  , Component (..)
  , StartOfFrame (..)
  , TreeType (..)
  , HuffmanTree (..)
  , getQType
  )
where

import Control.Exception (Exception (..))
import Data.Vector (Vector)
import Data.Word (Word8)

import Data.HuffmanTree

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

newtype Block = Block [BlockComponent]
  deriving (Eq, Show)

data BlockComponent = BlockComponent
  { blockComponentType :: ComponentType
  , blockValues :: Vector Int
  , blockQuantizationTableNumber :: Int
  }
  deriving (Eq, Show)

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
