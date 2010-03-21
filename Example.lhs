> {-# LANGUAGE TemplateHaskell, EmptyDataDecls, TypeFamilies, TypeOperators #-}
>
> module Example where
> 
> import Generics.Regular
> import Generics.Regular.Views
> import Generics.Regular.Formlets
> import Generics.Regular.JSON
> import qualified Generics.Regular.BlazeHtml as B
> import Text.JSON
> import Data.Record.Label
> import Control.Monad.Identity
> import Control.Applicative
> import qualified Text.XHtml.Strict as X
> import qualified Text.XHtml.Strict.Formlets as F
> import qualified Text.BlazeHtml.Html as Blaze
> import Text.BlazeHtml.Render.HtmlIO (renderHtmlIO)
> import System.IO (stdout)
> import Prelude hiding ((.))
> import Control.Category ((.))

Consider the following two datatypes @Person@ and @Place@:
 
> data Person = Person {
>    _name   :: String
>  , _age    :: Int
>  , _isMale :: Bool
>  , _place  :: Place
> } deriving (Show, Eq)

> data Place = Place {
>     _city      :: String
>   , _country   :: String
>   , _continent :: String
>   } deriving (Show, Eq)

> instance JSON Place where
>   readJSON = gfrom
>   showJSON = gto
>                
> instance JSON Person where
>   readJSON = gfrom
>   showJSON = gto

We can now derive a @Regular@ instance for the @Person@ datatype using Template
Haskell:

> $(deriveAll ''Place  "PFPlace")
> $(deriveAll ''Person "PFPerson")
> type instance PF Place  = PFPlace
> type instance PF Person = PFPerson

We can construct an example person:

> location :: Place
> location = Place "Utrecht" "The Netherlands" "Europe"
> chris    :: Person
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

> formHtml :: X.Html
> (_, Identity formHtml, _) = F.runFormState [] (personForm (Just chris))

This technique becomes even more powerful when we use the @fclabels@ package.
Suppose we want to display a form where only the @name@ and the @isMale@ can be
edited:

> data PersonView = PersonView {
>    __name   :: String
>  , __gender :: Gender
> }

> data Gender = Male | Female deriving (Eq, Show, Bounded, Enum)
> instance Formlet Gender where formlet = F.enumSelect []

> $(deriveAll ''PersonView "PFPersonView")
> type instance PF PersonView = PFPersonView

We can now use @fclabels@ to convert back and forth between @Person@ and
@PersonView@. First, we use TH to generate some accessor functions for us:

> $(mkLabels [''Person])

Now we need to write a bidirectional function between @Bool@ and @Gender@:

> genderBool :: Bool :<->: Gender
> genderBool = boolToGender <-> genderToBool
>  where  genderToBool Male   = True
>         genderToBool Female = False
>         boolToGender x      = if x then Male else Female

We can now write a bidirectional function between @Person@ and @PersonView@:

> toView :: Person :-> PersonView
> toView = Label (PersonView <$> __name `for` name <*> __gender `for` (genderBool `iso` isMale))

Now that we have a function with type @Person :-> PersonView@, we can render a
form for |personView| and update the original person. Note that the argument is
not a @Maybe@ value, in contrast with the @gformlet@ function.

> personForm' :: Person -> XForm Identity Person
> personForm' = projectedForm toView

> formHtml' :: X.Html
> (_, Identity formHtml', _) = F.runFormState [] (personForm' chris)

We can also generically generate JSON values. 

> chrisJSON :: JSValue
> chrisJSON = gto chris

> chrisFromJSON :: Result Person
> chrisFromJSON = gfrom chrisJSON

> testChrisFromJSON :: Bool
> testChrisFromJSON = case chrisFromJSON of
>   Ok x    -> x == chris
>   Error e -> False

> instance B.ToHtml Place where
>   toHtml = B.ghtml

> exampleBlaze :: IO ()
> exampleBlaze = renderHtmlIO stdout (B.ghtml chris)
