{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeOperators, TypeSynonymInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Generics.Regular.Formlets
-- Copyright   :  (c) 2010 Chris Eidhof
-- License     :  BSD3
--
-- Maintainer  :  chris@eidhof.nl
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Generic generation of formlets <http://hackage.haskell.org/package/formlets>. 
-- These functions are only defined for record datatypes that contain
-- a single constructor.
--
-- Consider the datatype @Person@:
--  
-- > data Person = Person {
-- >    _name   :: String
-- >  , _age    :: Int
-- >  , _isMale :: Bool
-- > } deriving (Show, Eq)
-- 
-- > $(deriveAll ''Person "PFPerson")
-- 
-- > type instance PF Person = PFPerson
-- 
-- We can construct an example person:
-- 
-- > chris    :: Person
-- > chris    = Person "chris" 25 True
-- 
-- > personForm :: XFormlet Identity Person
-- > personForm = gformlet
-----------------------------------------------------------------------------
module Generics.Regular.Formlets (
  -- * Generic forms
  gform,
  gformlet,
  GFormlet,
  -- * Generic forms with fclabels
  projectedForm,
  -- * Default Formlet typeclass
  Formlet (..),
  -- * Extra form types
  -- | Currently, the extra form types are very limited. We expect to add more types in the future, suggestions are welcome.
  YesNo (..),
  boolToYesNo
  ) where

import Control.Applicative
import Control.Monad.Identity
import Text.XHtml.Strict ((+++), (<<))
import qualified Text.XHtml.Strict as X
import qualified Text.XHtml.Strict.Formlets as F
import Generics.Regular
import Generics.Regular.Extras
import Data.Record.Label


gform :: (Regular a, GFormlet (PF a), Functor m, Applicative m, Monad m) => Maybe a -> XForm m a
gform x = to <$> (gformf gformlet (from <$> x))

gformlet :: (Regular a, GFormlet (PF a), Functor m, Applicative m, Monad m) => XFormlet m a
gformlet x = to <$> (gformf gformlet (from <$> x))

-- |
-- Generic forms almost never match the real world. If you want to change a generic form, you can either implement it from scratch, or use the 'projectedForm' function.
-- 
-- As an example, we will to remove the 'age' field from the form, and change the '_isMale' field to a Yes\/No choice instead of a True\/False choice. The datatype 'YesNo' is defined in this module.
-- 
-- > data PersonView = PersonView {
-- >    __name   :: String
-- >  , __isMale :: YesNo
-- > }
-- 
-- 
-- > $(deriveAll ''PersonView "PFPersonView")
-- > type instance PF PersonView = PFPersonView
-- 
-- We can now use @fclabels@ to convert back and forth between @Person@ and
-- @PersonView@. First, we use Template Haskell to generate some accessor functions:
-- 
-- > $(mkLabels [''Person])
-- 
-- This is the bidirectional function between @Person@ and @PersonView@. How to write such a function is explained in the well-documented @fclabels@ package at <http://hackage.haskell.org/package/fclabels>.
-- 
-- > toView :: Person :-> PersonView
-- > toView = Label (PersonView <$> __name `for` name <*> __isMale `for` (boolToYesNo . isMale))
-- 
-- Now that we have a function with type @Person :-> PersonView@, we can render a
-- form for @personView@ and update the original person. Note that the argument is
-- not a @Maybe@ value, in contrast with the @gformlet@ function.
-- 
-- > personForm' :: Person -> XForm Identity Person
-- > personForm' = projectedForm toView
-- 
-- > formHtml' :: X.Html
-- > (_, Identity formHtml', _) = F.runFormState [] (personForm' chris)

projectedForm :: (Regular a, GFormlet (PF a), Applicative m, Monad m) 
              => (b :-> a) -> b -> XForm m b
projectedForm toView x = (flip (set toView) x) <$> (gform (get toView <$> (Just x)))

type XForm m a = F.XHtmlForm m a
type XFormlet m a = F.XHtmlFormlet m a

class    Formlet a      where  formlet :: (Functor m, Applicative m, Monad m) => XFormlet m a

instance Formlet Bool   where  formlet   = F.enumSelect []
instance Formlet Int    where  formlet x = fromIntegral <$> F.inputInteger (toInteger <$> x)
instance Formlet String where  formlet   = F.input

class GFormlet f where
  gformf :: (Functor m, Applicative m, Monad m) => XFormlet m a -> XFormlet m (f a)

instance (Constructor c, GFormlet f) => GFormlet (C c f) where
  gformf f x = C <$> (gformf f $ unC <$> x)

instance Formlet a => GFormlet (K a) where
  gformf _ x = K <$> (formlet (unK <$> x))

instance (GFormlet (S s f), GFormlet g) => GFormlet ((S s f) :*: g) where
  gformf f x = (:*:) <$> (gformf f (prodFst <$> x)) <* F.xml X.br <*> (gformf f (prodSnd <$> x))

 
instance (Selector s, GFormlet f) => GFormlet (S s f) where
  gformf f x = F.plug ((X.label << (h (fromJust x) ++ ": ")) +++) $ S <$> gformf f (unS <$> x)
   where fromJust (Just y) = y
         fromJust _        = error "Generic formlets fromJust should not be computed."

-- | This datatype is used to display 'Bool' values as @Yes@ or @No@.
data YesNo = Yes | No 
  deriving (Eq, Show, Bounded, Enum)
instance Formlet YesNo where formlet = F.enumSelect []

-- | This is an @fclabels@ function that converts between 'Bool' and 'YesNo' values.
boolToYesNo :: Bool :-> YesNo
boolToYesNo = label to from
 where  from Yes _ = True
        from No  _ = False
        to x  = if x then Yes else No
