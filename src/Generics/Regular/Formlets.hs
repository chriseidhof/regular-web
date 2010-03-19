{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Generics.Regular.Formlets where

import Control.Applicative
import Control.Monad.Identity
import Text.XHtml.Strict ((+++), (<<))
import qualified Text.XHtml.Strict as X
import qualified Text.XHtml.Strict.Formlets as F
import Generics.Regular
import Generics.Regular.Extras
import Data.Record.Label

-- | A projectedForm' is used in combination with the @fclabels@ package. See the @Example.lhs@ file for an application.
projectedForm :: (Regular a, GFormlet (PF a), Applicative m, Monad m) 
              => (b :-> a) -> b -> XForm m b
projectedForm toView x = (flip (set toView) x) <$> (gform (get toView <$> (Just x)))

gform :: (Regular a, GFormlet (PF a), Functor m, Applicative m, Monad m) => Maybe a -> XForm m a
gform x = to <$> (gformf gformlet (from <$> x))

gformlet :: (Regular a, GFormlet (PF a), Functor m, Applicative m, Monad m) => XFormlet m a
gformlet x = to <$> (gformf gformlet (from <$> x))

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

instance (GFormlet f, GFormlet g) => GFormlet (f :*: g) where
  gformf f x = (:*:) <$> (gformf f (prodFst <$> x)) <* F.xml X.br <*> (gformf f (prodSnd <$> x))

 
instance (Selector s, GFormlet f) => GFormlet (S s f) where
  gformf f x = F.plug ((X.label << (h (fromJust x) ++ ": ")) +++) $ S <$> gformf f (unS <$> x)
   where fromJust (Just y) = y
         fromJust _        = error "Formlets fromJust should not be computed."
