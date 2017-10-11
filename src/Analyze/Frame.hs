{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Core frame types and functions
module Analyze.Frame where

-- import           Control.Monad       (join)
import           Control.Monad.Catch (MonadThrow (..))

import qualified Data.HashMap.Strict as HashMap
import           Data.HashMap.Strict (HashMap)
import           Data.Text           (Text)
import qualified Data.Vector         as Vector
import           Data.Vector         (Vector)

import           Analyze.Common

-- | In-memory row-oriented frame with columns named by `k` and values by `v`
data Frame r c = Frame
  { -- Ordered vector of row names
    _frameRowKeys   :: !(Vector r)
  , -- Ordered vector of column names
    _frameColumnKeys   :: !(Vector c)
  , -- Quick lookup from column name to column index
    _frameLookup :: !(HashMap c Int)
  , -- Vector of rows. Each element should be the length of number of columns.
    _frameData   :: !(Vector (Vector Text))
  } deriving (Eq, Read, Show)


-- | Get the vector of keys of a frame
keys :: Frame r c -> Vector c
keys = _frameColumnKeys


-- | Get the values of a frame
values :: Frame r c -> Vector (Vector Text)
values = _frameData


-- | An empty frame with no rows or columns
empty :: Frame r c
empty = Frame Vector.empty Vector.empty HashMap.empty Vector.empty


-- | Number of columns in an 'Frame'
countCols :: Frame r c -> Int
countCols (Frame _ cs _ _) = Vector.length cs


-- | Number of rows in an 'Frame'
countRows :: Frame r c -> Int
countRows (Frame rs _ _ _) = Vector.length rs


-- | Project to the given column, will throw an error if it cannot read the required type.
getColumn :: c -> v -> Frame r c -> Series r v
getColumn = undefined


-- | Takes first 'n' rows of an 'Frame'.
takeRows :: Int -> Frame r c -> Frame r c
takeRows n (Frame ks cs look vs) = Frame ks cs look (V.take n vs)


-- | Adds a 'Vector' column to the 'Frame'
addColumn :: (Data r, MonadThrow m) => Frame r c -> c -> Vector Text -> m (Frame r c)
addColumn = undefined

