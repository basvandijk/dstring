-----------------------------------------------------------------------------
-- |
-- Module      :  Data.DString
-- Copyright   :  (c) 2009 Bas van Dijk
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
-- Stability   :  experimental
-- Portability :  portable (Haskell 98)
--
-- Difference strings: a data structure for O(1) append on
-- strings. Note that a DString is just a newtype wrapper around a
-- 'DList Char'.  The reason we need a new type instead of just a type
-- synonym is that we can have an 'instance IsString DString' so we
-- can write overloaded string literals of type DString.
--
-----------------------------------------------------------------------------

module Data.DString
    ( DString

    -- * Conversion
    , fromDList
    , toDList
    , toString
    , fromShowS
    , toShowS

    -- * Basic functions
    , empty
    , singleton
    , cons
    , snoc
    , append
    , concat
    , list
    , head
    , tail
    , unfoldr
    , foldr
    )
    where

import Prelude hiding (concat, foldr, head, tail)
import qualified Data.DList as D
import Data.Monoid
import Data.String

newtype DString = DS {toDList :: D.DList Char}

-- | Convert a DList of Chars to a DString
fromDList :: D.DList Char -> DString
fromDList = DS

instance Monoid DString where
    mempty  = empty
    mappend = append

instance IsString DString where
    fromString = fromDList . D.fromList

-- | Convert a difference string back to a normal String
toString :: DString -> String
toString = D.toList . toDList

-- | Convert a ShowS to a difference string
fromShowS :: ShowS -> DString
fromShowS = fromDList . D.DL

-- | Convert a difference string to a ShowS
toShowS :: DString -> ShowS
toShowS = D.unDL . toDList

-- | Create a difference string containing no characters
empty :: DString
empty = fromDList D.empty

-- | Build a difference string from a single Char
singleton :: Char -> DString
singleton = fromDList . D.singleton

-- | /O(1)/, Prepend a Char to a difference string
cons :: Char -> DString -> DString
cons c ds = fromDList $ D.cons c (toDList ds)

-- | /O(1)/, Append a Char to a difference string
snoc :: DString -> Char -> DString
snoc ds c = fromDList $ D.snoc (toDList ds) c

-- | /O(1)/, Appending difference strings
append :: DString -> DString -> DString
x `append` y = fromDList (toDList x `D.append` toDList y)

-- | /O(spine)/, Concatenate difference strings
concat :: [DString] -> DString
concat = fromDList . D.concat . map toDList

-- | /O(length ds)/, difference list elimination, head, tail.
list :: b -> (Char -> DString -> b) -> DString -> b
list nill consit ds =
  case toString ds of
    []     -> nill
    x : xs -> consit x $ fromString xs

-- | Return the head of the difference string
head :: DString -> Char
head = list (error "Data.DString.head: empty list") const

-- | Return the tail of the difference string
tail :: DString -> DString
tail = list (error "Data.DString.tail: empty list") (flip const)

-- | Unfoldr for difference strings
unfoldr :: (b -> Maybe (Char, b)) -> b -> DString
unfoldr pf b = fromDList $ D.unfoldr pf b

-- | Foldr over difference strings
foldr  :: (Char -> b -> b) -> b -> DString -> b
foldr f b = D.foldr f b . toDList
