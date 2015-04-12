{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Aurora.Constraint where

import Control.Applicative (empty)
import Data.Text (Text)
import Text.PrettyPrint.Leijen (Pretty(..))

import Aurora.Optional (Optional)
import Aurora.Pretty (list', recordDoc, text)

data Constraint = Constraint
    { order :: Optional [Text]
    } deriving (Eq, Show)

-- | Default `Constraint`
_Constraint :: Constraint
_Constraint = Constraint
    { order = empty
    }

instance Pretty Constraint where
    pretty (Constraint{..}) = recordDoc "Constraint"
        [ ("order", fmap (list' text) order)
        ]
