{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Aurora.UpdateConfig where

import Control.Applicative (empty)
import Text.PrettyPrint.Leijen (Pretty(..), bool, integer)

import Aurora.Optional (Optional)
import Aurora.Pretty (recordDoc)

data UpdateConfig = UpdateConfig
    { batch_size                :: Optional Integer
    , restart_threshold         :: Optional Integer
    , watch_secs                :: Optional Integer
    , max_per_shard_failures    :: Optional Integer
    , max_total_failures        :: Optional Integer
    , rollback_on_failure       :: Optional Bool
    , wait_for_batch_completion :: Optional Bool
    , pulse_interval_secs       :: Optional Integer
    } deriving (Eq, Show)

-- | Default `UpdateConfig`
_UpdateConfig :: UpdateConfig
_UpdateConfig = UpdateConfig
    { batch_size                = empty
    , restart_threshold         = empty
    , watch_secs                = empty
    , max_per_shard_failures    = empty
    , max_total_failures        = empty
    , rollback_on_failure       = empty
    , wait_for_batch_completion = empty
    , pulse_interval_secs       = empty
    }

instance Pretty UpdateConfig where
    pretty (UpdateConfig{..}) = recordDoc "UpdateConfig"
        [ ("batch_size"               , fmap integer batch_size               )
        , ("restart_threshold"        , fmap integer restart_threshold        )
        , ("watch_secs"               , fmap integer watch_secs               )
        , ("max_per_shard_failures"   , fmap integer max_per_shard_failures   )
        , ("max_total_failures"       , fmap integer max_total_failures       )
        , ("rollback_on_failure"      , fmap bool    rollback_on_failure      )
        , ("wait_for_batch_completion", fmap bool    wait_for_batch_completion)
        , ("pulse_interval_secs"      , fmap integer pulse_interval_secs      )
        ]
