> {-# LANGUAGE TemplateHaskell, EmptyDataDecls, TypeFamilies, TypeOperators #-}
>
> module Example where
> 
> import Generics.Regular
> import Generics.Regular.Views
> import Generics.Regular.Formlets
> import Data.Record.Label
> import Control.Monad.Identity
> import Control.Applicative
> import qualified Text.XHtml.Strict as X
> import qualified Text.XHtml.Strict.Formlets as F

Consider the following two datatypes @Person@ and @Place@:
 
> data Person = Person {
>    _name   :: String
>  , _age    :: Int
>  , _isMale :: Bool
>  , _place  :: Place
> }

> data Place = Place {
>     _city      :: String
>   , _country   :: String
>   , _continent :: String
>   }

We can now derive a @Regular@ instance for the @Person@ datatype using Template
Haskell:

> $(deriveAll ''Place  "PFPlace")
> $(deriveAll ''Person "PFPerson")
>
> type instance PF Place  = PFPlace
> type instance PF Person = PFPerson

We can construct an example person:

> location = Place "Utrecht" "The Netherlands" "Europe"
> chris    = Person "chris" 25 True location

We can now generate some @Html@ for the @location@:

> example0 :: X.Html
> example0 = ghtml location

If we try to generate @Html@ for the @chris@ value, we get an error:

> -- No instance for (Html Place)

We can easily make @Place@ an instance of @Html@:

> instance Html Place where html = ghtml

More interestingly, we can generically build @Formlet@s this way:

> instance Formlet Place where formlet = gformlet

> personForm :: XFormlet Identity Person
> personForm = gformlet

We can print @formHtml@ to get the @Html@ of the form with the @chris@ value
already filled in:

> (_, Identity formHtml, _) = F.runFormState [] (personForm (Just chris))

This technique becomes even more powerful when we use the @fclabels@ package.
Suppose we want to display a form where only the @name@ and the @isMale@ can be
edited:

> data PersonView = PersonView {
>    __name   :: String
>  , __isMale :: Bool
> }

We can now use @fclabels@ to convert back and forth between @Person@ and
@PersonView@:

> $(mkLabels [''Person])

> toView :: Person :-> PersonView
> toView = Label (PersonView <$> __name `for` name <*> __isMale `for` isMale)

TODO: explain why this is nice.

Finally, we need to give an @Applicative@ instance for the @Identity@ monad.

> instance Applicative Identity where pure = return; (<*>) = ap
