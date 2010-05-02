{-# LANGUAGE UnicodeSyntax
           , NoImplicitPrelude
           , GeneralizedNewtypeDeriving
           , DeriveDataTypeable
  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.DString
-- Copyright   :  (c) 2009 Bas van Dijk
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
-- Stability   :  experimental
--
-- Difference strings: a data structure for O(1) append on strings. Note that a
-- 'DString' is just a newtype wrapper around a 'DList' 'Char'. The reason we
-- need a new type instead of just a type synonym is that we can have an
-- @instance 'IsString' 'DString'@ without using language extensions
-- (@TypeSynonymInstances@ or @FlexibleInstances@) so we can write overloaded
-- string literals of type 'DString'.
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
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import Prelude       ( fromInteger, (>=), error )
import Data.Char     ( Char )
import Data.Function ( ($), const, flip )
import Data.List     ( map )
import Data.Maybe    ( Maybe )
import Data.Monoid   ( Monoid )
import Data.Typeable ( Typeable )
import Data.String   ( IsString, fromString )
import Text.Show     ( Show, showsPrec, ShowS, showParen, showString, shows )

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) )

-- from to-string-class:
import Data.String.ToString ( ToString, toString )

-- from dlist:
import           Data.DList      ( DList(DL), unDL, toList, fromList )
import qualified Data.DList as D ( cons, snoc
                                 , foldr, unfoldr
                                 , singleton
                                 , concat
                                 )


--------------------------------------------------------------------------------
-- DString
--------------------------------------------------------------------------------

-- | A difference string is a function that given a string, returns the original
-- contents of the difference string prepended at the given string.
--
-- This structure supports O(1) @mappend@ en @snoc@ operations on strings making
-- it very usefull for append-heavy uses such as logging and pretty printing.
--
-- You can use it to efficiently show a tree for example: (Note that we use some
-- handy functions from the @string-combinators@ package)
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Data.DString (toShowS, fromShowS)
-- > import Data.String.Combinators ((<+>), paren)
-- >
-- > data Tree a = Leaf a | Branch (Tree a) (Tree a)
-- >
-- > instance Show a => Show (Tree a) where
-- >     showsPrec prec = showParen (prec >= funAppPrec) . toShowS . go
-- >         where
-- >           go (Leaf x)     = "Leaf" <+> fromShowS (showsPrec funAppPrec x)
-- >           go (Branch l r) = "Branch" <+> paren (go l) <+> paren (go r)
-- >
-- >           funAppPrec = 10
--
-- Note that a @DString@ can be converted from and to a 'String' using the
-- 'fromString' and 'toString' methods from the 'IsString' and 'ToString'
-- classes respectively.
newtype DString = DS (DList Char) deriving (Monoid, Typeable)

instance IsString DString where
    fromString = fromDList ∘ fromList

instance ToString DString where
    toString = toList ∘ toDList

instance Show DString where
    showsPrec p ds = showParen (p >= 10) $
                     showString "Data.String.fromString " ∘
                     shows (toString ds)


--------------------------------------------------------------------------------
-- Conversions
--------------------------------------------------------------------------------

-- | O(1) Convert a difference list of @Char@s to a difference string.
fromDList ∷ DList Char → DString
fromDList = DS

-- | O(1) Convert a difference string to a difference list.
toDList ∷ DString → DList Char
toDList (DS dl) = dl

-- | O(1) Convert a @ShowS@ to a difference string.
fromShowS ∷ ShowS → DString
fromShowS = fromDList ∘ DL

-- | O(1) Convert a difference string to a @ShowS@.
toShowS ∷ DString → ShowS
toShowS = unDL ∘ toDList


--------------------------------------------------------------------------------
-- Basic functions
--------------------------------------------------------------------------------

-- | O(1) Build a difference string from a single @Char@.
singleton ∷ Char → DString
singleton = fromDList ∘ D.singleton

-- | /O(1)/, Prepend a Char to a difference string.
cons ∷ Char → DString → DString
cons c ds = fromDList $ D.cons c (toDList ds)

-- | /O(1)/, Append a @Char@ to a difference string.
snoc ∷ DString → Char → DString
snoc ds c = fromDList $ D.snoc (toDList ds) c

-- | /O(spine)/, Concatenate difference strings.
concat ∷ [DString] → DString
concat = fromDList ∘ D.concat ∘ map toDList

-- | /O(length ds)/, difference list elimination, head, tail.
list ∷ α → (Char → DString → α) → DString → α
list nill consit ds =
  case toString ds of
    []     → nill
    x : xs → consit x $ fromString xs

-- | Return the head of the difference string.
head ∷ DString → Char
head = list (error "Data.DString.head: empty list") const

-- | Return the tail of the difference string.
tail ∷ DString → DString
tail = list (error "Data.DString.tail: empty list") (flip const)

-- | Unfoldr for difference strings.
unfoldr ∷ (α → Maybe (Char, α)) → α → DString
unfoldr pf b = fromDList $ D.unfoldr pf b

-- | Foldr over difference strings.
foldr  ∷ (Char → α → α) → α → DString → α
foldr f b = D.foldr f b ∘ toDList


-- The End ---------------------------------------------------------------------
