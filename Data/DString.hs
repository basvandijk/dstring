{-# LANGUAGE CPP
           , UnicodeSyntax
           , NoImplicitPrelude
           , DeriveDataTypeable
  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.DString
-- Copyright   :  (c) 2009-2011 Bas van Dijk
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
    , toString
    , fromDList
    , toDList
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

-- from base:
import Prelude       ( (>=), error )
import Data.Char     ( Char )
import Data.Function ( ($), const, flip )
import Data.List     ( map )
import Data.Maybe    ( Maybe )
import Data.Typeable ( Typeable )
import Data.String   ( IsString, fromString )
import Text.Show     ( Show, showsPrec, ShowS, showParen, showString, shows )

import Data.Semigroup as Sem
-- base >= 4.8: `Monoid` class is exported via `Prelude`
-- base < 4.11: re-exports `Monoid` class & common newtype wrappers
-- base >= 4.11: doesn't reexport `Monoid` class anymore

#if MIN_VERSION_base(4,11,0)
import Data.Monoid   ( Monoid, mempty, mappend )
#endif

#if MIN_VERSION_base(4,4,0)
import Data.String   ( String )
#else
import Data.Char     ( String )
#endif

#if __GLASGOW_HASKELL__ < 700
import Prelude       ( fromInteger )
#endif

-- from base-unicode-symbols:
import Data.Function.Unicode ( (∘) )

-- from dlist:
import           Data.DList      ( DList, apply, toList, fromList )
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
-- it very useful for append-heavy uses such as logging and pretty printing.
--
-- You can use it to efficiently show a tree for example: (Note that we use some
-- handy functions from the @string-combinators@ package)
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Data.DString (toShowS, fromShowS)
-- > import Data.String.Combinators ((<+>), parens, thenParens)
-- >
-- > data Tree a = Leaf a | Branch (Tree a) (Tree a)
-- >
-- > instance Show a => Show (Tree a) where
-- >     showsPrec prec t = toShowS $ (prec >= funAppPrec) `thenParens` go t
-- >         where
-- >           go (Leaf x)     = "Leaf" <+> fromShowS (showsPrec funAppPrec x)
-- >           go (Branch l r) = "Branch" <+> parens (go l) <+> parens (go r)
-- >
-- >           funAppPrec = 10
newtype DString = DS (DList Char) deriving (Typeable)

instance Show DString where
    showsPrec p ds = showParen (p >= 10) $
                     showString "Data.String.fromString " ∘
                     shows (toString ds)

instance Sem.Semigroup DString where
  ds1 <> ds2 = fromDList $ toDList ds1 `mappend` toDList ds2
  {-# INLINE (<>) #-}

instance Monoid DString where
  mempty = fromDList mempty
  {-# INLINE mempty #-}

#if !(MIN_VERSION_base(4,11,0))
  -- this is redundant starting with base-4.11 / GHC 8.4
  -- if you want to avoid CPP, you can define `mappend = (<>)` unconditionally
  mappend = (<>)
  {-# INLINE mappend #-}
#endif

--------------------------------------------------------------------------------
-- Conversions
--------------------------------------------------------------------------------

instance IsString DString where
    fromString = fromDList ∘ fromList
    {-# INLINE fromString #-}

-- | O(n) Convert a difference string to a normal string.
toString ∷ DString → String
toString = toList ∘ toDList
{-# INLINE toString #-}

-- | O(1) Convert a difference list of @Char@s to a difference string.
fromDList ∷ DList Char → DString
fromDList = DS
{-# INLINE fromDList #-}

-- | O(1) Convert a difference string to a difference list.
toDList ∷ DString → DList Char
toDList (DS dl) = dl
{-# INLINE toDList #-}

-- | O(1) Convert a difference string to a @ShowS@.
toShowS ∷ DString → ShowS
toShowS = apply ∘ toDList
{-# INLINE toShowS #-}


--------------------------------------------------------------------------------
-- Basic functions
--------------------------------------------------------------------------------

-- | O(1) Build a difference string from a single @Char@.
singleton ∷ Char → DString
singleton = fromDList ∘ D.singleton
{-# INLINE singleton #-}

-- | /O(1)/, Prepend a Char to a difference string.
cons ∷ Char → DString → DString
cons c ds = fromDList $ D.cons c (toDList ds)
{-# INLINE cons #-}

-- | /O(1)/, Append a @Char@ to a difference string.
snoc ∷ DString → Char → DString
snoc ds c = fromDList $ D.snoc (toDList ds) c
{-# INLINE snoc #-}

-- | /O(spine)/, Concatenate difference strings.
concat ∷ [DString] → DString
concat = fromDList ∘ D.concat ∘ map toDList
{-# INLINE concat #-}

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
{-# INLINE unfoldr #-}

-- | Foldr over difference strings.
foldr  ∷ (Char → α → α) → α → DString → α
foldr f b = D.foldr f b ∘ toDList
{-# INLINE foldr #-}
