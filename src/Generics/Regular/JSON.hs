{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
-- Summary: Functions for web programming.
-----------------------------------------------------------------------------


module Generics.Regular.JSON where

import Text.JSON
import Generics.Regular
import Control.Applicative
import Data.List (unionBy)

class GJSON f where
  gto   :: f a -> JSValue
  gfrom :: JSValue -> Result (f a)

instance GJSON U where
  gto   U = JSNull
  gfrom JSNull = Ok U
  gfrom _      = Error "could not parse U"

instance JSON a => GJSON (K a) where
  gto (K x) = showJSON x
  gfrom x = K <$> readJSON x

-- instance (GJSON f, GJSON g) => GJSON (f :+: g) where
--   gto (L x) = gto x
--   gto (R y) = gto y
--   gfrom x = let l = gfrom x 
--                 r = gfrom x
--             in maybe (fmap R r) (Just . L) l

instance (GJSON (S s f), GJSON g) => GJSON ((S s f) :*: g) where
  gto (a :*: b) = merge (gto a) (gto b)
  gfrom x       = do (:*:) <$> gfrom x <*> gfrom x

instance (Selector s, GJSON f) => GJSON (S s f) where
  gto s@(S x) = JSObject $ toJSObject [(selName s, gto x)]
  gfrom (JSObject obj) = let s = selName (undefined :: S s f x) 
                         in case valFromObj s obj of
                                 Ok x    -> S <$> gfrom x
                                 Error e -> Error e
  gfrom x              = Error $ "Expected json object, got " ++ show x

instance GJSON f => GJSON (C c f) where
  gto (C x) = gto x
  gfrom x   = C <$> gfrom x

merge :: JSValue -> JSValue -> JSValue
merge (JSObject l) (JSObject r) = JSObject (toJSObject $ mergeList (fromJSObject l) (fromJSObject r))
merge _            _            = error "Cannot merge objects."

mergeList :: [(String, JSValue)] -> [(String, JSValue)] -> [(String, JSValue)]
mergeList = unionBy (\x y -> fst x == fst y)
