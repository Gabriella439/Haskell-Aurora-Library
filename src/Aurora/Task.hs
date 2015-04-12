{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Aurora.Task where

import Control.Applicative (empty, pure)
import Data.Text (Text)
import Text.PrettyPrint.Leijen (Pretty(..), integer)

import Aurora.Constraint (Constraint)
import Aurora.Optional (Optional)
import Aurora.Process (Process)
import Aurora.Pretty (recordDoc, list', text)
import Aurora.Resources (Resources)

data Task = Task
    { name              :: Optional Text
    , processes         :: [Process]
    , constraints       :: Optional [Constraint]
    , resources         :: Resources
    , max_failures      :: Optional Integer
    , max_concurrency   :: Optional Integer
    , finalization_wait :: Optional Integer
    } deriving (Eq, Show)

instance Pretty Task where
    pretty (Task{..}) = recordDoc "Task"
        [ ("name"             , fmap  text                name              )
        , ("processes"        , fmap (list' pretty) (pure processes        ))
        , ("constraints"      , fmap (list' pretty)       constraints       )
        , ("resources"        , fmap  pretty        (pure resources        ))
        , ("max_failures"     , fmap  integer             max_failures      )
        , ("max_concurrency"  , fmap  integer             max_concurrency   )
        , ("finalization_wait", fmap  integer             finalization_wait )
        ]

{-| Default `Task`

    Required fields: `processes` and `resources`
-}
_Task :: Task
_Task = Task
    { name              = empty
    , constraints       = empty
    , max_failures      = empty
    , max_concurrency   = empty
    , finalization_wait = empty
    }
