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

    -- * ClientCluster
    , ClientCluster(..)
    , _ClientCluster
    , prettyClientClusters

    -- ** Authentication
    , Authentication(..)

    -- * Miscellaneous
    , Maximum(..)
    , Optional(..)

    -- * Re-exports
    , module Data.Word
    ) where

-- TODO: Create lenses and traversals for all types
-- TODO: Create defaults for all records
-- TODO: Document default values in haddocks
-- TODO: Document fields
-- TODO: Document the `NegativeLiterals` trick
-- TODO: Check that documentation examples compile
-- TODO: More type-safe units (i.e. bytes, seconds)
-- TODO: Pretty-printer should omit fields that match defaults
-- TODO: Documentation for Client cluster configuration seems inconsistent
-- TODO: Tighten imports

import Control.Applicative (Applicative(..), Alternative(..), liftA2)
import Control.Monad (MonadPlus(..))
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String(IsString(..))
import Data.Monoid (Monoid(..))
import Data.Word (Word, Word8, Word16)
import Text.PrettyPrint.Leijen hiding (empty)

-- | Options for a Thermos process
data Process = Process
    { processName                :: String
    -- ^ Process name
    , cmdline                    :: String
    -- ^ Command line
    , processPermissibleFailures :: Optional (Maximum Word)
    -- ^ Maximum permissible process run failures
    , daemon                     :: Optional Bool
    -- ^ When `True`, this is a daemon process
    , ephemeral                  :: Optional Bool
    -- ^ When `True`, this is an ephemeral process
    , minDuration                :: Optional Word
    -- ^ Minimum duration between process restarts in seconds
    , final                      :: Optional Bool
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
    name'        = pure (qString (processName p))
    cmdline'     = pure (qString (cmdline     p))
    maxFailures' = do
        m <- processPermissibleFailures p
        return (word (case m of
            Infinite -> 0
            Finite n -> n + 1 ))
    daemon'      = fmap bool (daemon      p)
    ephemeral'   = fmap bool (daemon      p)
    minDuration' = fmap word (minDuration p)
    final'       = fmap bool (final       p)

{-| Optional `Process`

    Required fields: `processName` and `cmdline`
-}
_Process :: Process
_Process = Process
    { processPermissibleFailures = empty
    , daemon                     = empty
    , ephemeral                  = empty
    , minDuration                = empty
    , final                      = empty
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

-- | Ordering `Constraint`s
_Constraint :: Constraint
_Constraint = Constraint { order = [] }

-- | Pretty print a `Constraint`
prettyConstraint :: Constraint -> Doc
prettyConstraint c = recordDoc
    "Constraint"
    [ ("order", order'')
    ]
  where
    order'' = pure (list' (map qString (order c)))

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

{-| Optional `Resource`

    Required fields: `cpu`, `ram`, and `disk`
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
    cpu'  = pure (double (cpu  r))
    ram'  = pure (word   (ram  r))
    disk' = pure (word   (disk r))

{-| Tasks fundamentally consist of a `taskName` and a `process`.  Processes can
    be further constrained with `taskConstraints`.  In Mesos, `resources` is
    also required.

-}
data Task = Task
    { taskName                :: Optional String
    -- ^ Optional user-supplied task name
    , process                 :: Process
    -- ^ First `Process` bound to this task
    , processes               :: [Process]
    -- ^ Remaining `Process`es bound to this task
    , taskConstraints         :: Optional [Constraint]
    -- ^ List of `Constraint`s constraining processes
    , resources               :: Resource
    -- ^ Resource footprint
    , taskPermissibleFailures :: Optional Word
    -- ^ Maximum permissible process failures
    , maxConcurrency          :: Optional (Maximum Word)
    -- ^ Maximum number of concurrent processes
    , finalizationWait        :: Optional Word
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
    name'             = fmap qString (taskName t)
    processes'        = pure (list' (map prettyProcess (process t:processes t)))
    constraints'      = fmap (list' . map prettyConstraint) (taskConstraints t)
    resources'        = pure (prettyResource (resources t))
    maxFailures'      = fmap word (taskPermissibleFailures t)
    maxConcurrency'   = fmap (word . f) (maxConcurrency t)
      where
        f x = case x of
            Infinite -> 0
            Finite n -> n + 1
    finalizationWait' = fmap word (finalizationWait t)
    
{-| Default `Task`

    Required fields: `process` and `resources`
-}
_Task :: Task
_Task = Task
    { taskName                = empty
    , processes               = []
    , taskConstraints         = empty
    , taskPermissibleFailures = empty
    , maxConcurrency          = empty
    , finalizationWait        = empty
    }

{-| Options for an aurora job

    You can find more extensive documentation at:

    <http://aurora.incubator.apache.org/documentation/latest/configuration-reference/>
-}
data Job = Job
    { task                   :: Task
    -- ^ The Task object to bind to this job
    , jobName                :: Optional String
    -- ^ Optional user-supplied job name
    , role                   :: String
    -- ^ Job role account
    , cluster                :: String
    -- ^ Cluster in which this job is scheduled
    , environment            :: Optional Environment
    -- ^ Job environment
    , contact                :: Optional String
    -- ^ Best email address to reach the owner of the job.  For production jobs,
    --   this is usually a team mailing list
    , instances              :: Optional Word
    -- ^ Number of instances (sometimes referred to as replicas or shards) of
    --   the task to create
    , updateConfig           :: Optional UpdateConfig
    -- ^ Parameters for controlling the rate and policy of rolling updates
    , jobConstraints         :: Optional (Map String String)
    -- ^ Scheduling constraints for the tasks
    , jobType                :: Optional JobType
    -- ^ Specify whether the job is a service or cron job
    , jobPermissibleFailures :: Optional (Maximum Word)
    -- ^ Maximum permissible task failures
    , priority               :: Optional Integer
    -- ^ Preemption priority to give the task.  Tasks with higher priorities may
    --   preempt tasks at lower priorities
    , production             :: Optional Bool
    -- ^ Whether or not this is a production task backed by quota.  Production
    --   jobs may preempt any non-production job, and may only be preempted by
    --   production jobs in the same role 
    , healthCheckConfig      :: Optional HealthCheckConfig
    -- ^ Parameters for controlling a task’s health checks via HTTP. Only used
    --   if a health port was assigned with a command line wildcard
    } deriving (Eq, Show)

{-| Default `Job`

    Required fields: `task`, `role`, `cluster`, and `contact`
-}
_Job :: Job
_Job = Job
    { jobName                = empty
    , environment            = empty
    , instances              = empty
    , updateConfig           = empty
    , jobConstraints         = empty
    , jobType                = empty
    , jobPermissibleFailures = empty
    , priority               = empty
    , production             = empty
    , healthCheckConfig      = empty
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
    task'                = pure (prettyTask        (task                   j))
    name'                = fmap  qString           (jobName                j)
    role'                = pure (qString           (role                   j))
    cluster'             = pure (qString           (cluster                j))
    environment'         = fmap  prettyEnvironment (environment            j)
    contact'             = fmap  qString           (contact                j)
    instances'           = fmap  word              (instances              j)
    cronSchedule'        = fmap  f                 (jobType                j)
      where
        f x = case x of
            Cron s _ -> prettySchedule s
            _        -> text "None"
    cronCollisionPolicy' = fmap  f                 (jobType                j)
      where
        f x = prettyCollisionPolicy (case x of
            Cron _ c -> c
            _        -> KillExisting )
    updateConfig'        = fmap prettyUpdateConfig (updateConfig           j)
    constraints'         = fmap f                  (jobConstraints         j)
      where
        f = jsonDoc . map (\(x, y) -> (x, pure (text y))) . Map.assocs
    service'             = fmap  f                 (jobType                j)
      where
        f x = bool (case x of
            Service -> True
            _       -> False )
    maxTaskFailures'     = fmap  f                 (jobPermissibleFailures j)
      where
        f x = int (case x of
            Infinite -> -1
            Finite n -> fromIntegral n + 1 )
    priority'            = fmap  integer                 (priority          j)
    production'          = fmap  bool                    (production        j)
    healthCheckConfig'   = fmap  prettyHealthCheckConfig (healthCheckConfig j)

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
    { batchSize                    :: Optional Word
    -- ^ Maximum number of shards to be updated in one iteration
    , restartThreshold             :: Optional Word
    -- ^ Maximum number of seconds before a shard must move into the `RUNNING`
    --   state before considered a failure 
    , watchSecs                    :: Optional Word
    -- ^ Minimum number of seconds a shard must remain in `RUNNING` state before
    --   considered a success 
    , perShardPermissibleFailures  :: Optional Word
    -- ^ Maximum number of permissible failures during update. Increments total
    --   failure count when this limit is exceeded
    , totalPermissibleFailures     :: Optional Word
    -- ^ Maximum number of shard failures to be tolerated in total during an
    --   update. Cannot be greater than or equal to the total number of tasks
    --   in a job
    } deriving (Eq, Show)

-- | Default `UpdateConfig`
_UpdateConfig :: UpdateConfig
_UpdateConfig = UpdateConfig
    { batchSize                   = empty
    , restartThreshold            = empty
    , watchSecs                   = empty
    , perShardPermissibleFailures = empty
    , totalPermissibleFailures    = empty
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
    batchSize'           = fmap word (batchSize                   u)
    restartThreshold'    = fmap word (restartThreshold            u)
    watchSecs'           = fmap word (watchSecs                   u)
    maxPerShardFailures' = fmap word (perShardPermissibleFailures u)
    maxTotalFailures'    = fmap word (totalPermissibleFailures    u)

-- | Parameters for controlling a task’s health checks via HTTP.
data HealthCheckConfig = HealthCheckConfig
    { initialIntervalSecs            :: Optional Word
    -- ^ Initial delay for performing an HTTP health check
    , intervalSecs                   :: Optional Word
    -- ^ Interval on which to check the task’s health via HTTP
    , timeoutSecs                    :: Optional Word
    -- ^ HTTP request timeout
    , consecutivePermissibleFailures :: Optional Word
    -- ^ Consecutive failures tolerated before considering a task unhealthy
    } deriving (Eq, Show)

-- | Default `HealthCheckConfig`
_HealthCheckConfig :: HealthCheckConfig
_HealthCheckConfig = HealthCheckConfig
    { initialIntervalSecs            = empty
    , intervalSecs                   = empty
    , timeoutSecs                    = empty
    , consecutivePermissibleFailures = empty
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
    initialIntervalSecs'    = fmap word (initialIntervalSecs            h)
    intervalSecs'           = fmap word (intervalSecs                   h)
    timeoutSecs'            = fmap word (timeoutSecs                    h)
    maxConsecutiveFailures' = fmap word (consecutivePermissibleFailures h)

{-| A cluster configuration file is used by the Aurora client to describe the
    Aurora clusters with which it can communicate.  Ultimately this allows
    client users to reference clusters with short names like @us-east@ and @eu@

    You can find more extensive documentation at:

    <http://aurora.incubator.apache.org/documentation/latest/client-cluster-configuration/>
-}
data ClientCluster = ClientCluster
    { clusterName       :: String
    -- ^ Cluster name
    , slaveRoot         :: String
    -- ^ Path to mesos slave work dir
    , slaveRunDirectory :: String
    -- ^ Name of mesos slave run dir
    , zk                :: Optional String
    -- ^ Hostname of ZooKeeper instance used to resolve Aurora schedulers
    , zkPort            :: Optional Word16
    -- ^ Port of ZooKeeper instance used to locate Aurora schedulers
    , schedulerZkPath   :: Optional String
    -- ^ ZooKeeper path under which scheduler instances are registered
    , schedulerUri      :: Optional String
    -- ^ URI of Aurora scheduler instance
    , proxyUrl          :: Optional String
    -- ^ Used by the client to format URLs for display
    , authMechanism     :: Optional Authentication
    -- ^ The authentication mechanism to use when communicating with the
    --   scheduler
    } deriving (Eq, Show)

{-| Default `ClientCluster`

    Required fields: `name`, `slaveRoot`, and `slaveRunDirectory`
-}
_ClientCluster :: ClientCluster
_ClientCluster = ClientCluster
    { zk              = empty
    , zkPort          = empty
    , schedulerZkPath = empty
    , schedulerUri    = empty
    , proxyUrl        = empty
    , authMechanism   = empty
    }

-- | Pretty print a `ClientCluster` as JSON
prettyClientCluster :: ClientCluster -> Doc
prettyClientCluster c = jsonDoc
    [ ("name"               , name'             )
    , ("slave_root"         , slaveRoot'        )
    , ("slave_run_directory", slaveRunDirectory')
    , ("zk"                 , zk'               )
    , ("zk_port"            , zkPort'           )
    , ("scheduler_zk_path"  , schedulerZkPath'  )
    , ("scheduler_uri"      , schedulerUri'     )
    , ("proxy_url"          , proxyUrl'         )
    , ("auth_mechanism"     , authMechanism'    )
    ]
  where
    name'              = pure (qString              (clusterName       c))
    slaveRoot'         = pure (qString              (slaveRoot         c))
    slaveRunDirectory' = pure (qString              (slaveRunDirectory c))
    zk'                = fmap  qString              (zk                c)
    zkPort'            = fmap  word16               (zkPort            c)
    schedulerZkPath'   = fmap  qString              (schedulerZkPath   c)
    schedulerUri'      = fmap  qString              (schedulerUri      c)
    proxyUrl'          = fmap  qString              (proxyUrl          c)
    authMechanism'     = fmap  prettyAuthentication (authMechanism     c)

-- | Pretty print a list of `ClientCluster`s as JSON
prettyClientClusters :: [ClientCluster] -> Doc
prettyClientClusters cs = list' (map prettyClientCluster cs)

-- | Authentication mechanism
data Authentication = Unauthenticated deriving (Eq, Show)

prettyAuthentication :: Authentication -> Doc
prettyAuthentication a = case a of
    Unauthenticated -> text "UNAUTHENTICATED"
    
-- | A potentially infinite value
data Maximum a = Infinite | Finite a deriving (Eq, Show)

instance Functor Maximum where
    fmap _  Infinite   = Infinite
    fmap f (Finite a ) = Finite (f a)

instance Applicative Maximum where
    pure = Finite

    Finite f <*> Finite x = Finite (f x)
    _        <*> _        = Infinite

instance Monad Maximum where
    return = Finite

    Infinite >>= _ = Infinite
    Finite x >>= f = f x

instance Alternative Maximum where
    empty = Infinite

    Infinite <|> x = x
    x        <|> _ = x

instance MonadPlus Maximum where
    mzero = empty
    mplus = (<|>)

instance Monoid a => Monoid (Maximum a) where
    mempty = pure mempty

    mappend = liftA2 mappend

instance Num a => Num (Maximum a) where
    fromInteger n = pure (fromInteger n)

    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)

    negate = fmap negate
    abs    = fmap abs
    signum = fmap signum

instance IsString a => IsString (Maximum a) where
    fromString str = pure (fromString str)

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

recordDoc :: String -> [(String, Optional Doc)] -> Doc
recordDoc recordName pairs = text recordName <> tupled' (concatMap format pairs)
  where
    format (fieldName, mValue) = case mValue of
        Default        -> []
        Specific value -> [text fieldName <+> equals <+> value]

jsonDoc :: [(String, Optional Doc)] -> Doc
jsonDoc pairs = braced (concatMap format pairs)
  where
    format (fieldName, mValue) = case mValue of
        Default        -> []
        Specific value -> [qString fieldName <> colon <+> value]

shown :: Show a => a -> Doc
shown = text . show

word :: Word -> Doc
word = shown

word16 :: Word16 -> Doc
word16 = shown

qString :: String -> Doc
qString = dquotes . text

list' :: [Doc] -> Doc
list' ds = list (map (space <>) ds)

tupled' :: [Doc] -> Doc
tupled' ds = tupled (map (space <>) ds)

braced :: [Doc] -> Doc
braced ds = encloseSep lbrace rbrace comma (map (space <>) ds)
