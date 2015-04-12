{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Aurora.HealthCheckConfig where

import Control.Applicative (empty)
import Text.PrettyPrint.Leijen (Pretty(..), integer)

import Aurora.Optional (Optional)
import Aurora.Pretty (recordDoc)

data HealthCheckConfig = HealthCheckConfig
    { initial_interval_secs    :: Optional Integer
    , interval_secs            :: Optional Integer
    , timeout_secs             :: Optional Integer
    , max_consecutive_failures :: Optional Integer
    } deriving (Eq, Show)

-- | Default `HealthCheckConfig`
_HealthCheckConfig :: HealthCheckConfig
_HealthCheckConfig = HealthCheckConfig
    { initial_interval_secs    = empty
    , interval_secs            = empty
    , timeout_secs             = empty
    , max_consecutive_failures = empty
    }

instance Pretty HealthCheckConfig where
    pretty (HealthCheckConfig{..}) = recordDoc "HealthCheckConfig"
        [ ("initial_interval_secs"   , fmap integer initial_interval_secs   )
        , ("interval_secs"           , fmap integer interval_secs           )
        , ("timeout_secs"            , fmap integer timeout_secs            )
        , ("max_consecutive_failures", fmap integer max_consecutive_failures)
        ]
