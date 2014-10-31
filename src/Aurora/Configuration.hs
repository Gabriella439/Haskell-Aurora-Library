{-# OPTIONS_GHC -fno-warn-missing-methods -fno-warn-missing-fields #-}

-- | Types and documentation related to Aurora configuration

module Aurora.Configuration (
    -- * Job
      Job(..)
    , _Job
    , prettyJob

    -- ** Task
    , Task(..)
    , _Task

    -- *** Process
    , Process(..)
    , _Process

    -- *** Constraint
    , Constraint(..)
    , _Constraint
    , order'

    -- *** Resource
    , Resource(..)
    , _Resource

    -- ** JobType
    , JobType(..)

    -- *** CollisionPolicy
    , CollisionPolicy(..)

    -- *** Cron
    , Schedule(..)
    , _Schedule

    -- **** Period
    , Period(..)

    -- **** Weekday
    , Weekday(..)

    -- **** Month
    , Month(..)

    -- ** Environment
    , Environment(..)

    -- ** UpdateConfig
    , UpdateConfig(..)
    , _UpdateConfig

    -- ** HealthCheckConfig
    , HealthCheckConfig(..)
    , _HealthCheckConfig

    -- * Maximum
    , Maximum(..)

    -- * Re-exports
    , Word
    ) where

-- TODO: Create lenses and traversals for all types
-- TODO: Create defaults for all records
-- TODO: Document default values in haddocks
-- TODO: Document fields
-- TODO: Document the `NegativeLiterals` trick
-- TODO: Check that documentation examples compile
-- TODO: More type-safe units (i.e. bytes, seconds)
-- TODO: Pretty-printer should omit fields that match defaults
-- TODO: Tighten imports

import Control.Applicative (Applicative(pure, (<*>)), liftA2, (<|>))
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word (Word, Word8)
import Text.PrettyPrint.Leijen

-- | Options for a Thermos process
data Process = Process
    { processName                :: String
    -- ^ Process name
    , cmdline                    :: String
    -- ^ Command line
    , processPermissibleFailures :: Maximum Word
    -- ^ Maximum permissible process run failures
    , daemon                     :: Bool
    -- ^ When `True`, this is a daemon process
    , ephemeral                  :: Bool
    -- ^ When `True`, this is an ephemeral process
    , minDuration                :: Word
    -- ^ Minimum duration between process restarts in seconds
    , final                      :: Bool
    -- ^ When `True`, this process is a finalizing one that should run last
    } deriving (Eq, Show)

-- | Pretty print a `Process`
prettyProcess :: Process -> Doc
prettyProcess p = recordDoc
    "Process"
    [ ("name"        , name'       )
    , ("cmdline"     , cmdline'    )
    , ("max_failures", maxFailures')
    , ("daemon"      , daemon'     )
    , ("ephemeral"   , ephemeral'  )
    , ("min_duration", minDuration')
    , ("final"       , final'      )
    ]
  where
    name'        = qString (processName p)
    cmdline'     = qString (cmdline     p)
    maxFailures' = word (case processPermissibleFailures p of
        Unlimited -> 0
        Finite n  -> n + 1 )
    daemon'      = bool    (daemon      p)
    ephemeral'   = bool    (daemon      p)
    minDuration' = word    (minDuration p)
    final'       = bool    (final       p)

{-| Default `Process`

    Required fields: `processName` and `cmdline`

> _Process = Process
>     { processPermissibleFailures = 0
>     , daemon                     = False
>     , ephemeral                  = False
>     , minDuration                = 15
>     , final                      = False
>     }
-}
_Process :: Process
_Process = Process
    { processPermissibleFailures = 0
    , daemon                     = False
    , ephemeral                  = False
    , minDuration                = 15
    , final                      = False
    }

{-| Current constraint objects only support a single ordering constraint,
    `order`, which specifies its processes run sequentially in the order given.
    By default, all processes run in parallel when bound to a `Task` without
    ordering constraints.
-}
data Constraint = Constraint
    { order :: [String]
    -- ^ List of processes by name that should be run serially
    } deriving (Eq, Show)

{-| Default `Constraint`

> _Constraint = Constraint { order = [] }
-}
_Constraint :: Constraint
_Constraint = Constraint { order = [] }

-- | Pretty print a `Constraint`
prettyConstraint :: Constraint -> Doc
prettyConstraint c = recordDoc
    "Constraint"
    [ ("order", order'')
    ]
  where
    order'' = list' (map qString (order c))

{-| Use the `order'` function as shorthand to generate Constraint lists. The
    following:

> order' process1 process2

    ... is shorthand for:

> [Constraint { order = [processName process1, processName process2] }
-}
order' :: Process -> Process -> [Constraint]
order' process1 process2 =
    [Constraint { order = [processName process1, processName process2] }]

-- | Specifies the amount of CPU, Ram, and disk resources the task needs
data Resource = Resource
    { cpu  :: Double
    -- ^ Fractional number of cores required by the task.
    , ram  :: Word
    -- ^ Bytes of RAM required by the task
    , disk :: Word
    -- ^ Bytes of disk required by the task
    } deriving (Eq, Show)

{-| Default `Resource`

    Required fields: `cpu`, `ram`, and `disk`

> _Resource = Resource {}
-}
_Resource :: Resource
_Resource = Resource {}

-- | Pretty print a `Resource`
prettyResource :: Resource -> Doc
prettyResource r = recordDoc
    "Resource"
    [ ("cpu" , cpu' )
    , ("ram" , ram' )
    , ("disk", disk')
    ]
  where
    cpu'  = double (cpu  r)
    ram'  = word   (ram  r)
    disk' = word   (disk r)

{-| Tasks fundamentally consist of a `taskName` and a `process`.  Processes can
    be further constrained with `taskConstraints`.  In Mesos, `resources` is
    also required.

-}
data Task = Task
    { taskName                :: Maybe String
    -- ^ Optional user-supplied task name
    , process                 :: Process
    -- ^ First `Process` bound to this task
    , processes               :: [Process]
    -- ^ Remaining `Process`es bound to this task
    , taskConstraints         :: [Constraint]
    -- ^ List of `Constraint`s constraining processes
    , resources               :: Resource
    -- ^ Resource footprint
    , taskPermissibleFailures :: Word
    -- ^ Maximum permissible process failures
    , maxConcurrency          :: Maximum Word
    -- ^ Maximum number of concurrent processes
    , finalizationWait        :: Word
    -- ^ Amount of time allocated for finalizing processes, in seconds
    } deriving (Eq, Show)

-- | Pretty print a `Task`
prettyTask :: Task -> Doc
prettyTask t = recordDoc
    "Task"
    [ ("name"             , name'            )
    , ("processes"        , processes'       )
    , ("constraints"      , constraints'     )
    , ("resources"        , resources'       )
    , ("max_failures"     , maxFailures'     )
    , ("max_concurrency"  , maxConcurrency'  )
    , ("finalization_wait", finalizationWait')
    ]
  where
    name'             = string (case taskName t of
        Nothing  -> processName (process t)
        Just str -> str )
    processes'        = list' (map prettyProcess (process t : processes t))
    constraints'      = list' (map prettyConstraint (taskConstraints t))
    resources'        = prettyResource (resources t)
    maxFailures'      = word (taskPermissibleFailures t)
    maxConcurrency'   = word (case maxConcurrency t of
        Unlimited -> 0
        Finite n  -> n + 1 )
    finalizationWait' = word (finalizationWait t)
    
{-| Default `Task`

    Required fields: `process` and `resources`

> _Task = Task
>     { taskName                = Nothing
>     , processes               = []
>     , taskConstraints         = []
>     , taskPermissibleFailures = 0
>     , maxConcurrency          = Unlimited
>     , finalizationWait        = 30
>     }
-}
_Task :: Task
_Task = Task
    { taskName                = Nothing
    , processes               = []
    , taskConstraints         = []
    , taskPermissibleFailures = 0
    , maxConcurrency          = Unlimited
    , finalizationWait        = 30
    }

-- | Options for an aurora job
data Job = Job
    { task                   :: Task
    -- ^ The Task object to bind to this job
    , jobName                :: Maybe String
    -- ^ Optional user-supplied job name
    , role                   :: String
    -- ^ Job role account
    , cluster                :: String
    -- ^ Cluster in which this job is scheduled
    , environment            :: Environment
    -- ^ Job environment
    , contact                :: String
    -- ^ Best email address to reach the owner of the job.  For production jobs,
    --   this is usually a team mailing list
    , instances              :: Word
    -- ^ Number of instances (sometimes referred to as replicas or shards) of
    --   the task to create
    , updateConfig           :: UpdateConfig
    -- ^ Parameters for controlling the rate and policy of rolling updates
    , jobConstraints         :: Map String String
    -- ^ Scheduling constraints for the tasks
    , jobType                :: JobType
    -- ^ Specify whether the job is a service or cron job
    , jobPermissibleFailures :: Maximum Word
    -- ^ Maximum permissible task failures
    , priority               :: Integer
    -- ^ Preemption priority to give the task.  Tasks with higher priorities may
    --   preempt tasks at lower priorities
    , production             :: Bool
    -- ^ Whether or not this is a production task backed by quota.  Production
    --   jobs may preempt any non-production job, and may only be preempted by
    --   production jobs in the same role 
    , healthCheckConfig      :: HealthCheckConfig
    -- ^ Parameters for controlling a task’s health checks via HTTP. Only used
    --   if a health port was assigned with a command line wildcard
    } deriving (Eq, Show)

{-| Default `Job`

    Required fields: `task`, `role`, `cluster`, and `contact`

> _Job = Job
>     { jobName                = Nothing
>     , environment            = Devel
>     , instances              = 1
>     , updateConfig           = _UpdateConfig
>     , jobConstraints         = Map.empty
>     , jobType                = Ordinary
>     , jobPermissibleFailures = 0
>     , priority               = 0 
>     , production             = False
>     , healthCheckConfig      = _HealthCheckConfig
>     }
-}
_Job :: Job
_Job = Job
    { jobName                = Nothing
    , environment            = Devel
    , instances              = 1
    , updateConfig           = _UpdateConfig
    , jobConstraints         = Map.empty
    , jobType                = Ordinary
    , jobPermissibleFailures = 0
    , priority               = 0 
    , production             = False
    , healthCheckConfig      = _HealthCheckConfig
    }

-- | Pretty print a `Job`
prettyJob :: Job -> Doc
prettyJob j = recordDoc
    "Job"
    [ ("task"                 , task'               )
    , ("name"                 , name'               )
    , ("role"                 , role'               )
    , ("cluster"              , cluster'            )
    , ("environment"          , environment'        )
    , ("contact"              , contact'            )
    , ("instances"            , instances'          )
    , ("cron_schedule"        , cronSchedule'       )
    , ("cron_collision_policy", cronCollisionPolicy')
    , ("update_config"        , updateConfig'       )
    , ("constraints"          , constraints'        )
    , ("service"              , service'            )
    , ("max_task_failures"    , maxTaskFailures'    )
    , ("priority"             , priority'           )
    , ("production"           , production'         )
    , ("health_check_config"  , healthCheckConfig'  )
    ]
  where
    task'                = prettyTask              (task              j)
    name'                = qString (case jobName j <|> taskName (task j) of
        Nothing  -> processName (process (task j))
        Just str -> str )
    role'                = qString                 (role              j)
    cluster'             = qString                 (cluster           j)
    environment'         = prettyEnvironment       (environment       j)
    contact'             = qString                 (contact           j)
    instances'           = word                    (instances         j)
    cronSchedule'        = case jobType j of
        Cron s _ -> prettySchedule s
        _        -> text "None"
    cronCollisionPolicy' = prettyCollisionPolicy (case jobType j of
        Cron _ c -> c
        _        -> KillExisting )
    updateConfig'        = prettyUpdateConfig (updateConfig j)
    constraints'         = dict (map format (Map.assocs (jobConstraints j)))
      where
        dict = encloseSep lbrace rbrace comma
        format (key, value) = text key <+> colon <+> text value
    service'             = bool (case jobType j of
        Service -> True
        _       -> False )
    maxTaskFailures'     = int (case jobPermissibleFailures j of
        Unlimited -> -1
        Finite n  -> fromIntegral n + 1 )
    priority'            = integer                 (priority          j)
    production'          = bool                    (production        j)
    healthCheckConfig'   = prettyHealthCheckConfig (healthCheckConfig j)

{-| Specify whether the job is a service or cron job

    Services are differentiated from non-service Jobs in that tasks always
    restart on completion, whether successful or unsuccessful. Jobs that are not
    services may only retry `taskPermissibleFailures` times.
-}
data JobType
    = Ordinary
    -- ^ Default job type
    | Service
    -- ^ Long-running service
    | Cron Schedule CollisionPolicy
    -- ^ Periodic command
    deriving (Eq, Show)

-- | How to handle existing cron jobs
data CollisionPolicy
    = KillExisting
    -- ^ Kill any existing jobs in progress
    | CancelNew
    -- ^ Let any existing jobs finish
    deriving (Eq, Show)

-- | Pretty print a `CollisionPolicy`
prettyCollisionPolicy :: CollisionPolicy -> Doc
prettyCollisionPolicy c = text (case c of
    KillExisting -> "KILL_EXISTING"
    CancelNew    -> "CANCEL_NEW" )

{-| A `Cron` schedule

    Some example translations

> Schedule (Every 15) All All All All          -- */15 * * * *
> Schedule 59 23 31 December Friday            -- 59 23 31 12 5
> Schedule All All All (At [January,May]) All  -- * * * 1,5 *
> Schedule All All All (Range January May) All -- * * * 1-5 *
-}
data Schedule = Schedule
    { minutes  :: Period Word8
    , hours    :: Period Word8
    , days     :: Period Word8
    , months   :: Period Month
    , weekdays :: Period Weekday
    } deriving (Eq, Show)

{-| Default `Schedule`

> _Schedule = Schedule
>     { minutes  = All
>     , hours    = All
>     , days     = All
>     , months   = All
>     , weekdays = All
>     }
-}
_Schedule :: Schedule
_Schedule = Schedule
    { minutes  = All
    , hours    = All
    , days     = All
    , months   = All
    , weekdays = All
    }

-- | Pretty print a `Schedule`
prettySchedule :: Schedule -> Doc
prettySchedule s = minutes' <+> hours' <+> days' <+> months' <+> weekdays'
  where
    fieldWith :: (Schedule -> Period Word8) -> Doc
    fieldWith accessor = case accessor s of
        All         -> text "*"
        Every n     -> text ("*/" ++ show n)
        At ns       -> text (intercalate "," (map show ns))
        Range n1 n2 -> text (show n1 ++ "-" ++ show n2)

    minutes'  = fieldWith minutes
    hours'    = fieldWith hours
    days'     = fieldWith days
    months'   = fieldWith (fmap (fromIntegral . succ . fromEnum) . months  )
    weekdays' = fieldWith (fmap (fromIntegral . succ . fromEnum) . weekdays)

-- | One field of a `Schedule`
data Period n
    = All
    -- ^ @*@
    | Every n
    -- ^ @*/n@
    | At [n]
    -- ^ @n1,n2,n3@
    | Range n n
    -- ^ @n1-n2@
    deriving (Eq, Show)

instance Num n => Num (Period n) where
    fromInteger n = At [fromInteger n]

instance Functor Period where
    fmap f p = case p of
        All         -> All
        Every n     -> Every (f n)
        At    ns    -> At (map f ns)
        Range n1 n2 -> Range (f n1) (f n2)

-- | Day of the week
data Weekday
    = Sunday
    | Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | Month of the year
data Month
    = January
    | February
    | March
    | April
    | May
    | June
    | July
    | August
    | September
    | October
    | November
    | December
    deriving (Eq, Ord, Show, Enum, Bounded)

-- | Aurora environment to deploy to
data Environment
    = Devel
    -- ^ For development
    | Test
    -- ^ For tests
    | Staging Int
    -- ^ Staging environment, before a production deploy
    | Prod
    -- ^ Production
    deriving (Eq, Show)

-- | Pretty print an `Environment`
prettyEnvironment :: Environment -> Doc
prettyEnvironment e = qString (case e of
    Devel     -> "devel"
    Test      -> "test"
    Staging n -> "staging" ++ show n
    Prod      -> "prod" )

-- | Parameters for controlling the rate and policy of rolling updates
data UpdateConfig = UpdateConfig
    { batchSize                    :: Word
    -- ^ Maximum number of shards to be updated in one iteration
    , restartThreshold             :: Word
    -- ^ Maximum number of seconds before a shard must move into the `RUNNING`
    --   state before considered a failure 
    , watchSecs                    :: Word
    -- ^ Minimum number of seconds a shard must remain in `RUNNING` state before
    --   considered a success 
    , perShardPermissibleFailures  :: Word
    -- ^ Maximum number of permissible failures during update. Increments total
    --   failure count when this limit is exceeded
    , totalPermissibleFailures     :: Word
    -- ^ Maximum number of shard failures to be tolerated in total during an
    --   update. Cannot be greater than or equal to the total number of tasks
    --   in a job
    } deriving (Eq, Show)

{-| Default `UpdateConfig`

> _UpdateConfig :: UpdateConfig
> _UpdateConfig = UpdateConfig
>     { batchSize                   = 1
>     , restartThreshold            = 60
>     , watchSecs                   = 45
>     , perShardPermissibleFailures = 0
>     , totalPermissibleFailures    = 0
>     }
-}
_UpdateConfig :: UpdateConfig
_UpdateConfig = UpdateConfig
    { batchSize                   = 1
    , restartThreshold            = 60
    , watchSecs                   = 45
    , perShardPermissibleFailures = 0
    , totalPermissibleFailures    = 0
    }

-- | Pretty print an `UpdateConfig`
prettyUpdateConfig :: UpdateConfig -> Doc
prettyUpdateConfig u = recordDoc
    "UpdateConfig"
    [ ("batch_size"            , batchSize'          )
    , ("restart_threshold"     , restartThreshold'   )
    , ("watch_secs"            , watchSecs'          )
    , ("max_per_shard_failures", maxPerShardFailures')
    , ("max_total_failures"    , maxTotalFailures'   )
    ]
  where
    batchSize'           = word (batchSize                   u)
    restartThreshold'    = word (restartThreshold            u)
    watchSecs'           = word (watchSecs                   u)
    maxPerShardFailures' = word (perShardPermissibleFailures u)
    maxTotalFailures'    = word (totalPermissibleFailures    u)

-- | Parameters for controlling a task’s health checks via HTTP.
data HealthCheckConfig = HealthCheckConfig
    { initialIntervalSecs            :: Word
    -- ^ Initial delay for performing an HTTP health check
    , intervalSecs                   :: Word
    -- ^ Interval on which to check the task’s health via HTTP
    , timeoutSecs                    :: Word
    -- ^ HTTP request timeout
    , consecutivePermissibleFailures :: Word
    -- ^ Consecutive failures tolerated before considering a task unhealthy
    } deriving (Eq, Show)

{-| Default `HealthCheckConfig`
-}
_HealthCheckConfig :: HealthCheckConfig
_HealthCheckConfig = HealthCheckConfig
    { initialIntervalSecs            = 15
    , intervalSecs                   = 10
    , timeoutSecs                    = 1
    , consecutivePermissibleFailures = 0
    }

-- | Pretty print a `HealthCheckConfig`
prettyHealthCheckConfig :: HealthCheckConfig -> Doc
prettyHealthCheckConfig h = recordDoc
    "HealthCheckConfig"
    [ ("initial_interval_secs"   , initialIntervalSecs'   )
    , ("interval_secs"           , intervalSecs'          )
    , ("timeout_secs"            , timeoutSecs'           )
    , ("max_consecutive_failures", maxConsecutiveFailures')
    ]
  where
    initialIntervalSecs'    = word (initialIntervalSecs            h)
    intervalSecs'           = word (intervalSecs                   h)
    timeoutSecs'            = word (timeoutSecs                    h)
    maxConsecutiveFailures' = word (consecutivePermissibleFailures h)

{-| A potentially unlimited value

    Note that `Maximum` implements `Num`, so you can use naked numeric literals
    anywhere something expects a `Maximum` and the compiler will automatically
    wrap the numeric literal in `Finite`
-}
data Maximum a = Unlimited | Finite a deriving (Eq, Show)

instance Functor Maximum where
    fmap _  Unlimited  = Unlimited
    fmap f (Finite a ) = Finite (f a)

instance Applicative Maximum where
    pure = Finite

    Finite f <*> Finite x = Finite (f x)
    _        <*> _        = Unlimited

instance Monad Maximum where
    return = Finite

    Unlimited >>= _ = Unlimited
    Finite x  >>= f = f x

instance Num a => Num (Maximum a) where
    fromInteger a = Finite (fromInteger a)

    (+) = liftA2 (+)

    (*) = liftA2 (*)

    negate = fmap negate

    signum = fmap signum

    abs    = fmap abs

recordDoc :: String -> [(String, Doc)] -> Doc
recordDoc recordName pairs =
        text recordName <> tupled' (map format pairs)
  where
    format (fieldName, value) = text fieldName <+> equals <+> value

shown :: Show a => a -> Doc
shown = text . show

word :: Word -> Doc
word = shown

qString :: String -> Doc
qString = dquotes . text

list' :: [Doc] -> Doc
list' ds = list (map (space <>) ds)

tupled' :: [Doc] -> Doc
tupled' ds = tupled (map (space <>) ds)
