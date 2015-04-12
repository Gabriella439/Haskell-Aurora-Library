module Aurora.Optional where

import Control.Applicative (Applicative(..), Alternative(..), liftA2)
import Control.Monad (MonadPlus(..))
import Data.String (IsString(..))
import Data.Monoid (Monoid(..))

-- | A type that will `Default` to a particular value if left unspecified
data Optional a = Default | Specific a deriving (Eq, Show)

instance Functor Optional where
    fmap f x = case x of
        Default    -> Default
        Specific y -> Specific (f y)

instance Applicative Optional where
    pure = Specific

    Specific f <*> Specific x = Specific (f x)
    _          <*> _          = Default

instance Monad Optional where
    return = Specific

    Default    >>= _ = Default
    Specific x >>= f = f x

instance Alternative Optional where
    empty = Default

    Default <|> x = x
    x       <|> _ = x

instance MonadPlus Optional where
    mzero = empty
    mplus = (<|>)

instance Monoid a => Monoid (Optional a) where
    mempty = pure mempty

    mappend = liftA2 mappend

instance Num a => Num (Optional a) where
    fromInteger n = pure (fromInteger n)

    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)

    negate = fmap negate
    abs    = fmap abs
    signum = fmap signum

instance IsString a => IsString (Optional a) where
    fromString str = pure (fromString str)
