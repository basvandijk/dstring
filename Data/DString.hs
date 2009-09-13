{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.DString
-- Copyright   :  (c) 2009 Bas van Dijk
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
-- Stability   :  experimental
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
    , fromShowS
    , toShowS

    -- * Basic functions
    , singleton
    , cons
    , snoc
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
import Data.Monoid (Monoid)
import Data.String (IsString, fromString)
import qualified Data.String.ToString as ToString


--------------------------------------------------------------------------------
-- DString
--------------------------------------------------------------------------------

-- | A difference string is a function that given a string, returns
-- the original contents of the difference string prepended at the
-- given string.
--
-- This structure supports O(1) @append@ en @snoc@ operations on
-- strings making it very usefull for append-heavy uses such as
-- logging and pretty printing.
--
-- You can use it to efficiently show a tree for example:
-- (Note that we make use of some functions from the /string-combinators/ package:
-- <http://hackage.haskell.org/cgi-bin/hackage-scripts/package/string-combinators>)
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Data.DString
-- > import Data.String.Combinators ((<+>), fromShow, paren)
-- >
-- > data Tree a = Leaf a | Branch (Tree a) (Tree a)
-- >
-- > instance Show a => Show (Tree a) where
-- >     show = toString . go
-- >         where
-- >           go (Leaf x)     = "Leaf" <+> fromShow x
-- >           go (Branch l r) = "Branch" <+> paren (go l) <+> paren (go r)
newtype DString = DS { toDList :: D.DList Char -- ^ Convert a difference string to a difference list
                     } deriving Monoid

instance IsString DString where
    fromString = fromDList . D.fromList

instance ToString.ToString DString where
    toString = D.toList . toDList

instance Show DString where
    showsPrec p ds = showParen (p >= 10) $
                     showString "Data.String.fromString " .
                     shows (ToString.toString ds)


--------------------------------------------------------------------------------
-- Conversions
--------------------------------------------------------------------------------

-- | Convert a difference list of Chars to a difference string
fromDList :: D.DList Char -> DString
fromDList = DS

-- | Convert a ShowS to a difference string
fromShowS :: ShowS -> DString
fromShowS = fromDList . D.DL

-- | Convert a difference string to a ShowS
toShowS :: DString -> ShowS
toShowS = D.unDL . toDList


--------------------------------------------------------------------------------
-- Basic functions
--------------------------------------------------------------------------------

-- | Build a difference string from a single Char
singleton :: Char -> DString
singleton = fromDList . D.singleton

-- | /O(1)/, Prepend a Char to a difference string
cons :: Char -> DString -> DString
cons c ds = fromDList $ D.cons c (toDList ds)

-- | /O(1)/, Append a Char to a difference string
snoc :: DString -> Char -> DString
snoc ds c = fromDList $ D.snoc (toDList ds) c

-- | /O(spine)/, Concatenate difference strings
concat :: [DString] -> DString
concat = fromDList . D.concat . map toDList

-- | /O(length ds)/, difference list elimination, head, tail.
list :: b -> (Char -> DString -> b) -> DString -> b
list nill consit ds =
  case ToString.toString ds of
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


-- The End ---------------------------------------------------------------------
