{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Regular.Views
-- Copyright   :  (c) 2010 Chris Eidhof
-- License     :  BSD3
--
-- Maintainer  :  chris@eidhof.nl
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Summary: Functions for generating HTML.
-----------------------------------------------------------------------------


module Generics.Regular.Views (
  -- * Generic HTML generation.
  ghtml, 
  Html (..),
  GHtml
  -- gtable, 
  -- gtableRow,
  -- Table (..),
  -- GTable
  ) where

import Text.XHtml.Strict ((+++), (<<))
import qualified Text.XHtml.Strict as X

import Generics.Regular
import Generics.Regular.Extras

-- | The function 'ghtml' converts an 'a' value into 'X.Html'
ghtml :: (Regular a, GHtml (PF a)) => a -> X.Html
ghtml x = ghtmlf ghtml (from x)

-- | The function 'gtable' converts a list of 'a's into an 'X.Html' table with a row for each element.
gtable :: (Regular a, GTable (PF a)) => [a] -> X.Html
gtable xs = X.table << map gtableRow xs

-- | The class 'Html' converts a simple value 'a' into 'X.Html'.
class Html a where
  html :: a -> X.Html

instance Html Float  where html = X.toHtml . show
instance Html Int    where html = X.toHtml . show
instance Html Bool   where html = X.toHtml . show
instance Html String where html = X.toHtml 

-- | The class 'GHtml' converts a simple value 'a' into 'X.Html'.
class GHtml f where
  ghtmlf :: (a -> X.Html) -> f a -> X.Html

instance GHtml I where
  ghtmlf f (I r) = f r

instance (Constructor c, GHtml f) => GHtml (C c f) where
  ghtmlf f cx@(C x) = (X.h1 << capitalize (conName cx)) +++ ghtmlf f x

instance Html a => GHtml (K a) where
  ghtmlf _ (K x) = html x

instance (GHtml f, GHtml g) => GHtml (f :*: g) where
  ghtmlf f (x :*: y) = ghtmlf f x +++ X.br +++ ghtmlf f y

instance (GHtml f, GHtml g) => GHtml (f :+: g) where
  ghtmlf f (L x) = ghtmlf f x
  ghtmlf f (R y) = ghtmlf f y

instance (Selector s, GHtml f) => GHtml (S s f) where
  ghtmlf f s@(S x) = X.label << ((h s) ++ ": ") +++ ghtmlf f x


class Table a where
  table :: a -> X.Html

instance Table Float  where table = html
instance Table String where table = html
instance Table Int    where table = html
instance Table Bool   where table = html

class GTable f where
  gtablef :: (a -> X.Html) -> f a -> X.Html

instance GTable I where
  gtablef f (I r) = f r

instance (Constructor c, GTable f) => GTable (C c f) where
  gtablef f (C x) = X.tr << (gtablef f x)

instance Table a => GTable (K a) where
  gtablef _ (K x) = table x

instance (GTable f, GTable g) => GTable (f :*: g) where
  gtablef f (x :*: y) = gtablef f x +++ gtablef f y

instance (Selector s, GTable f) => GTable (S s f) where
  gtablef f (S x) = X.td << gtablef f x

gtableRow :: (Regular a, GTable (PF a)) => a -> X.Html
gtableRow x = gtablef gtableRow (from x)
