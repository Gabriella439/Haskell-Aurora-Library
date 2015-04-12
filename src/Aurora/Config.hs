-- | This module is a convenience module that re-exports all the other Aurora
-- configuration.  Due to field name conflicts, not all fields may be exported,
-- but in most common cases, you can get away with just:
--
-- > import Aurora.Config
--
-- In some rarer cases, you may need to add these two imports as well:
--
-- > import qualified Aurora.Job  as Job   -- For `constraints` and `name` fields
-- > import qualified Aurora.Task as Task  -- For `max_failures` and `name` fields
--
-- Here is an example of how to build a minimal Aurora `Job` using the provided
-- types:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Aurora.Config
-- >
-- > job :: Job
-- > job = _Job
-- >     { task    = _Task
-- >         { processes =
-- >             [ _Process
-- >                 { name    = "hello"
-- >                 , cmdline = "echo Hello, world!"
-- >                 }
-- >             ]
-- >         , resources = _Resources
-- >             { cpu  = 1
-- >             , ram  = 1 * gb
-- >             , disk = 1 * gb
-- >             }
-- >         }
-- >     , role    = "example-role"
-- >     , cluster = "example-cluster"
-- >     , contact = "example@example.com"
-- >     }
--
-- You can then render the above `Job` to a valid Aurora configuration file using
-- `renderJobs`:
--
-- >>> Data.Text.IO.putStrLn (renderJobs [job])
-- jobs = [ Job( task = Task( processes = [ Process( name = "hello"
--                                                 , cmdline = "echo Hello, world!")]
--                          , resources = Resources( cpu = 1.0
--                                                 , ram = 1073741824
--                                                 , disk = 1073741824))
--             , role = "example-role"
--             , cluster = "example-cluster"
--             , contact = "example@example.com")]
--
-- Some things to note:
--
-- * The `Optional` type implements `IsString`, so you can use a string literal
--   wherever you see `Optional` `Text`
-- * The `Optional` type implements `Num`, so you can use a numeric literal where
--   you see `Optional` `Integer` or `Optional` `Double`
-- * Any `Optional` field that you leave `empty` will not be rendered in the final
--   Aurora config, using whatever Aurora defaults to for that field

module Aurora.Config (
    -- * Render
      renderJobs

    -- * Job
    , Job(..)
    , _Job

    -- ** Task
    , Task(..)
    , _Task

    -- *** Process
    , Process(..)
    , _Process

    -- *** Constraint
    , Constraint(..)
    , _Constraint

    -- *** Resources
    , Resources(..)
    , _Resources

    -- ** UpdateConfig
    , UpdateConfig(..)
    , _UpdateConfig

    -- ** HealthCheckConfig
    , HealthCheckConfig(..)
    , _HealthCheckConfig

    -- ** Container
    , Container(..)
    , _Container

    -- *** Docker
    , Docker(..)
    , _Docker

    -- * Optional
    , Optional(..)

    -- * Constants
    , bytes
    , kb
    , mb
    , gb
    , tb

    -- * Re-exports
    , Text
    ) where

import Data.Text (Text)

import Aurora.Constants
import Aurora.Constraint
import Aurora.Container
import Aurora.Docker
import Aurora.HealthCheckConfig
import Aurora.Job hiding (constraints, name)
import Aurora.Optional
import Aurora.Process
import Aurora.Resources
import Aurora.Task hiding (max_failures, name)
import Aurora.UpdateConfig
