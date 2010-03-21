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


module Generics.Regular.BlazeHtml (
  -- * Generic HTML generation.
  ghtml, 
  ToHtml (..),
  GHtml
  ) where

import Text.BlazeHtml.Html as B
import Text.BlazeHtml.Text as T

import Generics.Regular
import Generics.Regular.Extras

import Data.Monoid

-- | The function 'ghtml' converts an 'a' value into 'X.Html'
ghtml :: (Regular a, GHtml (PF a), B.Html h) => a -> h
ghtml x = ghtmlf ghtml (from x)

-- | The class 'Html' converts a simple value 'a' into 'X.Html'.
class ToHtml a where
  toHtml :: B.Html h => a -> h

packText :: B.Html h => String -> h
packText = B.text . T.pack

instance ToHtml Float  where toHtml = packText . show
instance ToHtml Int    where toHtml = packText . show
instance ToHtml Bool   where toHtml = packText . show
instance ToHtml String where toHtml = packText 

-- | The class 'GHtml' converts a simple value 'a' into 'X.Html'.
class GHtml f where
  ghtmlf :: B.Html h => (a -> h) -> f a -> h

instance GHtml I where
  ghtmlf f (I r) = f r

instance (Constructor c, GHtml f) => GHtml (C c f) where
  ghtmlf f cx@(C x) = (B.h1 $ packText $ capitalize (conName cx)) `mappend` ghtmlf f x

instance ToHtml a => GHtml (K a) where
  ghtmlf _ (K x) = toHtml x

instance (GHtml f, GHtml g) => GHtml (f :*: g) where
  ghtmlf f (x :*: y) = ghtmlf f x `mappend` B.br `mappend` ghtmlf f y

instance (GHtml f, GHtml g) => GHtml (f :+: g) where
  ghtmlf f (L x) = ghtmlf f x
  ghtmlf f (R y) = ghtmlf f y

instance (Selector s, GHtml f) => GHtml (S s f) where
  ghtmlf f s@(S x) = B.label (packText $ (h s) ++ ": ") `mappend` ghtmlf f x
