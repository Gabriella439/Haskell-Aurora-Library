{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Aurora.Job where

import Data.Text (Text, pack)

import Control.Applicative (empty, pure)
import Text.PrettyPrint.Leijen hiding (empty, text)

import Aurora.Container (Container)
import Aurora.HealthCheckConfig (HealthCheckConfig)
import Aurora.Optional (Optional)
import Aurora.Pretty (recordDoc, label, list', text)
import Aurora.Task (Task)
import Aurora.UpdateConfig (UpdateConfig)

-- | Visit the "Aurora.Job" module for properly formatted documentation for `Job`
data Job = Job
    { task                  :: Task
    , name                  :: Optional Text
    , role                  :: Text
    , cluster               :: Text
    , environment           :: Optional Text
    , contact               :: Text
    , instances             :: Optional Integer
    , cron_schedule         :: Optional Text
    , cron_collision_policy :: Optional Text
    , update_config         :: Optional UpdateConfig
    , constraints           :: Optional [(Text, Text)]
    , service               :: Optional Bool
    , max_task_failures     :: Optional Integer
    , priority              :: Optional Integer
    , production            :: Optional Bool
    , health_check_config   :: Optional HealthCheckConfig
    , container             :: Optional Container
    } deriving (Eq, Show)

{-| Default `Job`

    Required fields: `task`, `role`, `cluster`, and `contact`
-}
_Job :: Job
_Job = Job
    { name                   = empty
    , environment            = empty
    , instances              = empty
    , cron_schedule          = empty
    , cron_collision_policy  = empty
    , update_config          = empty
    , constraints            = empty
    , service                = empty
    , max_task_failures      = empty
    , priority               = empty
    , production             = empty
    , health_check_config    = empty
    , container              = empty
    }

instance Pretty Job where
    pretty (Job {..}) = recordDoc "Job"
        [ ("task"                 , fmap pretty  (pure task                 ))
        , ("name"                 , fmap text          name                  )
        , ("role"                 , fmap text    (pure role                 ))
        , ("cluster"              , fmap text    (pure cluster              ))
        , ("environment"          , fmap text          environment           )
        , ("contact"              , fmap text    (pure contact              ))
        , ("instances"            , fmap integer       instances             )
        , ("cron_schedule"        , fmap text          cron_schedule         )
        , ("cron_collision_policy", fmap text          cron_collision_policy )
        , ("update_config"        , fmap pretty        update_config         )
        , ("constraints"          , fmap constraint    constraints           )
        , ("service"              , fmap bool          service               )
        , ("max_task_failures"    , fmap integer       max_task_failures     )
        , ("priority"             , fmap integer       priority              )
        , ("production"           , fmap bool          production            )
        , ("health_check_config"  , fmap pretty        health_check_config   )
        , ("container"            , fmap pretty        container             )
        ]

constraint :: [(Text, Text)] -> Doc
constraint keyVals = encloseSep lbrace rbrace comma (map (space <>) keyVals')
  where
    keyVals' = do
        (key, val) <- keyVals
        return (text key <> colon <+> text val)

-- | Render anything that implements `Pretty` as `Text`
renderJobs :: [Job] -> Text
renderJobs jobs = pack (show (label "jobs" <+> equals <+> list' pretty jobs))
