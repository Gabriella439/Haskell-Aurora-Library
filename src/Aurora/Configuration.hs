{-# OPTIONS_GHC -fno-warn-missing-methods #-}

-- | Types and documentation related to Aurora configuration

module Aurora.Configuration (
    -- * Job
      Job(..)
    , _Job
    , prettyJob
    , task
    , jobName
    , role
    , cluster
    , environment
    , contact
    , instances
    , updateConfig
    , jobConstraints
    , jobType
    , jobPermissibleFailures
    , priority
    , production
    , healthCheckConfig

    -- ** Task
    , Task(..)
    , _Task
    , taskName
    , process
    , processes
    , taskConstraints
    , resources
    , taskPermissibleFailures
    , maxConcurrency
    , finalizationWait

    -- *** Process
    , Process(..)
    , _Process
    , processName
    , cmdline
    , processPermissibleFailures
    , daemon
    , ephemeral
    , minDuration
    , final

    -- *** Constraint
    , Constraint(..)
    , _Constraint
    , order
    , order'

    -- *** Resource
    , Resource(..)
    , _Resource
    , cpu
    , ram
    , disk

    -- ** JobType
    , JobType(..)
    , _Ordinary
    , _Service
    , _Cron

    -- *** CollisionPolicy
    , CollisionPolicy(..)
    , _KillExisting
    , _CancelNew

    -- *** Cron
    , Schedule(..)
    , _Schedule
    , minutes
    , hours
    , days
    , months
    , weekdays

    -- **** Period
    , Period(..)
    , _All
    , _Every
    , _At
    , _Range

    -- **** Weekday
    , Weekday(..)
    , _Sunday
    , _Monday
    , _Tuesday
    , _Wednesday
    , _Thursday
    , _Friday
    , _Saturday

    -- **** Month
    , Month(..)
    , _January
    , _February
    , _March
    , _April
    , _May
    , _June
    , _July
    , _August
    , _September
    , _October
    , _November
    , _December

    -- ** Environment
    , Environment(..)
    , _Devel
    , _Test
    , _Staging
    , _Prod

    -- ** UpdateConfig
    , UpdateConfig(..)
    , _UpdateConfig
    , batchSize
    , restartThreshold
    , watchSecs
    , perShardPermissibleFailures
    , totalPermissibleFailures

    -- ** HealthCheckConfig
    , HealthCheckConfig(..)
    , _HealthCheckConfig
    , initialIntervalSecs
    , intervalSecs
    , timeoutSecs
    , consecutivePermissibleFailures

    -- * Maximum
    , Maximum(..)
    , _Unlimited
    , _Finite

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
    { _processName                :: String
    -- ^ Process name
    , _cmdline                    :: String
    -- ^ Command line
    , _processPermissibleFailures :: Maximum Word
    -- ^ Maximum permissible process run failures
    , _daemon                     :: Bool
    -- ^ When `True`, this is a daemon process
    , _ephemeral                  :: Bool
    -- ^ When `True`, this is an ephemeral process
    , _minDuration                :: Word
    -- ^ Minimum duration between process restarts in seconds
    , _final                      :: Bool
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
    name'        = qString (_processName p)
    cmdline'     = qString (_cmdline     p)
    maxFailures' = word (case _processPermissibleFailures p of
        Unlimited -> 0
        Finite n  -> n + 1 )
    daemon'      = bool    (_daemon      p)
    ephemeral'   = bool    (_daemon      p)
    minDuration' = word    (_minDuration p)
    final'       = bool    (_final       p)

{-| Default `Process`

    Required fields: `processName` and `cmdline`

> _Process = Process
>     { _processPermissibleFailures = 0
>     , _daemon                     = False
>     , _ephemeral                  = False
>     , _minDuration                = 15
>     , _final                      = False
>     }
-}
_Process :: Process
_Process = Process
    { _processPermissibleFailures = 0
    , _daemon                     = False
    , _ephemeral                  = False
    , _minDuration                = 15
    , _final                      = False
    }

{-|
> processName :: Lens' Process String

    The name is any valid UNIX filename string (specifically no slashes, NULLs
    or leading periods).  Within a task object, each process name must be
    unique.
-}
processName :: Functor f => (String -> f String) -> (Process -> f Process)
processName k x = fmap (\y -> x { _processName = y }) (k (_processName x))

{-|
> cmdline :: Lens' Process String

    The command line run by the process. The command line is invoked in a bash
    subshell, so can involve fully-blown bash scripts.  However, nothing is
    supplied for command-line arguments so @$*@ is unspecified.
-}
cmdline :: Functor f => (String -> f String) -> (Process -> f Process)
cmdline k x = fmap (\y -> x { _cmdline = y }) (k (_processName x))

{-|
> processPermissibleFailures :: Lens' Process (Maximum Word)

    The maximum number of times the process can fail without being marked
    permanently failed.  When a process permanently fails it counts as one
    failure against `taskPermissibleFailures`.

    Set `processPermissibleFailures` to `Unlimited` to let the process retry
    indefinitely.  It retries at most once every `minDuration` seconds to
    prevent an effective denial of service attack on the coordinating Thermos
    scheduler.
-}
processPermissibleFailures
    :: Functor f
    => (Maximum Word -> f (Maximum Word)) -> (Process -> f Process)
processPermissibleFailures k x =
    fmap (\y -> x { _processPermissibleFailures = y })
         (k (_processPermissibleFailures x))

{-|
> daemon :: Lens' Process Bool

    By default, Thermos processes are non-daemon. If `daemon` is set to `True`,
    a successful (zero) exit status does not prevent future process runs.
    Instead, the process reinvokes after `minDuration` seconds.  However, the
    maximum failure limit still applies.  A combination of @`_daemon`=`True`@
    and @`_processPermissibleFailures`=`Unlimited`@ causes a process to retry
    indefinitely regardless of exit status.  This should be avoided for very
    short-lived processes because of the accumulation of checkpointed state for
    each process run.  When running in Mesos specifically,
    `processPermissibleFailures` is capped at 99.
-}
daemon :: Functor f => (Bool -> f Bool) -> (Process -> f Process)
daemon k x = fmap (\y -> x { _daemon = y }) (k (_daemon x))

{-|
> ephemeral :: Lens' Process Bool

    By default, Thermos processes are non-ephemeral.  If `ephemeral` is set to
    `True`, the process' status is not used to determine if its containing task
    has completed.  For example, consider a task with a non-ephemeral webserver
    process and an ephemeral logsaver process that periodically checkpoints its
    log files to a centralized data store.  The task is considered finished once
    the webserver process has completed, regardless of the logsaver’s current
    status.
-}
ephemeral :: Functor f => (Bool -> f Bool) -> (Process -> f Process)
ephemeral k x = fmap (\y -> x { _ephemeral = y }) (k (_ephemeral x))

{-|
> minDuration :: Lens' Process Word

    Processes may succeed or fail multiple times during a single task’s
    duration. Each of these is called a process run. `minDuration` is the
    minimum number of seconds the scheduler waits before running the same
    process.
-}
minDuration :: Functor f => (Word -> f Word) -> (Process -> f Process)
minDuration k x = fmap (\y -> x { _minDuration = y }) (k (_minDuration x))

{-|
> final :: Lens' Process Bool

    Processes can be grouped into two classes: ordinary processes and finalizing
    processes.  By default, Thermos processes are ordinary.  They run as long as
    the task is considered healthy (i.e., no failure limits have been reached.)
    But once all regular Thermos processes finish or the task reaches a certain
    failure threshold, it moves into a \"finalization\" stage and runs all
    finalizing processes.  These are typically processes necessary for cleaning
    up the task, such as log checkpointers, or perhaps e-mail notifications that
    the task completed.

    Finalizing processes may not depend upon ordinary processes or vice-versa,
    however finalizing processes may depend upon other finalizing processes and
    otherwise run as a typical process schedule.
-}
final :: Functor f => (Bool -> f Bool) -> (Process -> f Process)
final k x = fmap (\y -> x { _final = y }) (k (_final x))

{-| Current constraint objects only support a single ordering constraint,
    `order`, which specifies its processes run sequentially in the order given.
    By default, all processes run in parallel when bound to a `Task` without
    ordering constraints.
-}
data Constraint = Constraint
    { _order :: [String]
    -- ^ List of processes by name that should be run serially
    } deriving (Eq, Show)

{-| Default `Constraint`

> _Constraint = Constraint { _order = [] }
-}
_Constraint :: Constraint
_Constraint = Constraint { _order = [] }

-- | Pretty print a `Constraint`
prettyConstraint :: Constraint -> Doc
prettyConstraint c = recordDoc
    "Constraint"
    [ ("order", order')
    ]
  where
    order' = list' (map qString (_order c))

{-|
> order :: Lens' Constraint String
-}
order :: Functor f => ([String] -> f [String]) -> (Constraint -> f Constraint)
order k x = fmap (\y -> x { _order = y }) (k (_order x))

{-| Use the `order'` function as shorthand to generate Constraint lists. The
    following:

> order' process1 process2

    ... is shorthand for:

> [Constraint { _order = [process1 ^. processName, process2 ^. processName] }
-}
order' :: Process -> Process -> [Constraint]
order' process1 process2 =
    [Constraint { _order = [_processName process1, _processName process2] }]

-- | Specifies the amount of CPU, Ram, and disk resources the task needs
data Resource = Resource
    { _cpu  :: Double
    -- ^ Fractional number of cores required by the task.
    , _ram  :: Word
    -- ^ Bytes of RAM required by the task
    , _disk :: Word
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
    cpu'  = double (_cpu  r)
    ram'  = word   (_ram  r)
    disk' = word   (_disk r)

{-|
> cpu :: Lens' Resource Double
-}
cpu :: Functor f => (Double -> f Double) -> (Resource -> f Resource)
cpu k x = fmap (\y -> x { _cpu = y }) (k (_cpu x))

{-|
> ram :: Lens' Resource Word
-}
ram :: Functor f => (Word -> f Word) -> (Resource -> f Resource)
ram k x = fmap (\y -> x { _ram = y }) (k (_ram x))

{-|
> disk :: Lens' Resource Word
-}
disk :: Functor f => (Word -> f Word) -> (Resource -> f Resource)
disk k x = fmap (\y -> x { _disk = y }) (k (_disk x))

{-| Tasks fundamentally consist of a `taskName` and a `process`.  Processes can
    be further constrained with `taskConstraints`.  In Mesos, `resources` is
    also required.

-}
data Task = Task
    { _taskName                :: Maybe String
    -- ^ Optional user-supplied task name
    , _process                 :: Process
    -- ^ First `Process` bound to this task
    , _processes               :: [Process]
    -- ^ Remaining `Process`es bound to this task
    , _taskConstraints         :: [Constraint]
    -- ^ List of `Constraint`s constraining processes
    , _resources               :: Resource
    -- ^ Resource footprint
    , _taskPermissibleFailures :: Word
    -- ^ Maximum permissible process failures
    , _maxConcurrency          :: Maximum Word
    -- ^ Maximum number of concurrent processes
    , _finalizationWait        :: Word
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
    name'             = string (case _taskName t of
        Nothing  -> _processName (_process t)
        Just str -> str )
    processes'        = list' (map prettyProcess (_process t : _processes t))
    constraints'      = list' (map prettyConstraint (_taskConstraints t))
    resources'        = prettyResource (_resources t)
    maxFailures'      = word (_taskPermissibleFailures t)
    maxConcurrency'   = word (case _maxConcurrency t of
        Unlimited -> 0
        Finite n  -> n + 1 )
    finalizationWait' = word (_finalizationWait t)
    
{-| Default `Task`

    Required fields: `taskName` and `resources`

> _Task = Task
>     { _taskName                = Nothing
>     , _processes               = []
>     , _taskConstraints         = []
>     , _taskPermissibleFailures = 0
>     , _maxConcurrency          = Unlimited
>     , _finalizationWait        = 30
>     }
-}
_Task :: Task
_Task = Task
    { _taskName                = Nothing
    , _processes               = []
    , _taskConstraints         = []
    , _taskPermissibleFailures = 0
    , _maxConcurrency          = Unlimited
    , _finalizationWait        = 30
    }

{-|
> taskName :: Lens' Task String

    `taskName` is an optional user-supplied string denoting the name of this
    task.  If you supply `Nothing` then the task will default to
    @`process` `.` `processName`@.
-}
taskName :: Functor f => (Maybe String -> f (Maybe String)) -> (Task -> f Task)
taskName k x = fmap (\y -> x { _taskName = y }) (k (_taskName x))

{-|
> process :: Lens' Task Process

    First `Process`, used to statically enforce that the user supplies at least
    one `Process`
-}
process :: Functor f => (Process -> f Process) -> (Task -> f Task)
process k x = fmap (\y -> x { _process = y }) (k (_process x))

{-|
> processes :: Lens' Task [Process]

    Used to specify optional additional `Process`es beyond the mandatory first
    `Process`
-}
processes :: Functor f => ([Process] -> f [Process]) -> (Task -> f Task)
processes k x = fmap (\y -> x { _processes = y }) (k (_processes x))

{-|
> taskConstraints :: Lens' Task [Constraint]

    A list of `Constraint` objects. Currently it supports only one type, the
    `order` constraint.  `order` is a list of process names that should run in
    the order given. For example:

> process = Process { _cmdline = "echo hello {{name}}" }
>
> task = Task
>     { _taskName        = "echoes"
>     , _process         =  process { _processName = "jim" }
>     , _processes       = [process { _processName = "bob" }]
>     , _taskConstraints = [Constraint { _order = ["jim", "bob"] }]
>     }

    Constraints can be supplied ad-hoc and in duplicate. Not all Processes need
    be constrained, however Tasks with cycles are rejected by the Thermos
    scheduler.
-}
taskConstraints
    :: Functor f => ([Constraint] -> f [Constraint]) -> (Task -> f Task)
taskConstraints k x =
    fmap (\y -> x { _taskConstraints = y }) (k (_taskConstraints x))

{-|
> resources :: Lens' Task Resource

    Takes a `Resource` object, which specifies the amounts of CPU, memory, and
    disk space resources to allocate to the Task.
-}
resources :: Functor f => (Resource -> f Resource) -> (Task -> f Task)
resources k x = fmap (\y -> x { _resources = y }) (k (_resources x))

{-|
> taskPermissibleFailures :: Lens' Task (Maximum Word)

    `taskPermissibleFailures` is the number of times processes may permanently
    fail without the task failing

    For example:

> template = _Process { _processPermissibleFailures = 9 }
>
> task = _Task
>     { _taskName    = "fail"
>     , _processes   =
>         [ template { _processName = "failing"   , _cmdline = "exit 1" }
>         , template { _processName = "succeeding", _cmdline = "exit 0" }
>         ]
>     , _taskPermissibleFailures = 1
>     }

    The @failing@ Process would be marked permanently failed after 10 failed
    runs, and the succeeding Process would succeed on the first run.  The task
    would succeed because the task permits 1 process marked permanently failed.
-}
taskPermissibleFailures :: Functor f => (Word -> f Word) -> (Task -> f Task)
taskPermissibleFailures k x =
    fmap (\y -> x { _taskPermissibleFailures = y })
         (k (_taskPermissibleFailures x))

{-|
> maxConcurrency :: Lens' Task (Maximum Word)

    For Tasks with a number of expensive but otherwise independent processes,
    you may want to limit the amount of concurrency the Thermos scheduler
    provides rather than artificially constraining it via `order` constraints.
    For example, a test framework may generate a task with 100 test run
    processes, but wants to run it on a machine with only 4 cores. You can limit
    the amount of parallelism to 4 by setting @`_max_concurrency` = 4@ in your
    task configuration.

    For example, the following task spawns 180 Processes (\"mappers\") to
    compute individual elements of a 180 degree sine table, all dependent upon
    one final Process (\"reducer\") to tabulate the results:

> makeMapper id = Process
>     { _processName = "mapper" ++ show id
>     , _cmdline     = "echo 'scale=50;s(" ++ show id ++ "*4*a(1)/180)' \
>                      \ | bc -l > temp.sine_table." ++ show id
>     }
>
> makeReducer = Process
>     { _processName = "reducer"
>     , _cmdline     = "cat temp.* | nl > sine_table.txt && rm -f temp.*"
>     }
>
> processes = map makeMapper [1..180]
>
> task = _Task
>     { _taskName        = "mapreduce"
>     , _process         = makeReducer
>     , _processes       = processes
>     , _taskConstraints =
>         [ Constraint { _order = [mapper ^. processName, "reducer"]
>         | mapper <- processes
>         ]
>     , _maxConcurrency = 8
>     }
-}
maxConcurrency
    :: Functor f => (Maximum Word -> f (Maximum Word)) -> (Task -> f Task)
maxConcurrency k x =
    fmap (\y -> x { _maxConcurrency = y }) (k (_maxConcurrency x))

{-|
> finalizationWait :: Lens' Task Word

    Tasks have three active stages: @ACTIVE@, @CLEANING@, and @FINALIZING@. The
    @ACTIVE@ stage is when ordinary processes run. This stage lasts as long as
    Processes are running and the Task is healthy. The moment either all
    Processes have finished successfully or the Task has reached a maximum
    Process failure limit, it goes into @CLEANING@ stage and send SIGTERMs to
    all currently running Processes and their process trees. Once all Processes
    have terminated, the Task goes into @FINALIZING@ stage and invokes the
    schedule of all Processes with the “final” attribute set to True.

    This whole process from the end of @ACTIVE@ stage to the end of @FINALIZING@
    must happen within finalization_wait seconds. If it does not finish during
    that time, all remaining Processes are sent SIGKILLs (or if they depend upon
    uncompleted Processes, are never invoked.)

    Client applications with higher priority may force a shorter finalization
    wait (e.g. through parameters to @thermos kill@), so this is mostly a
    best-effort signal.
-}
finalizationWait :: Functor f => (Word -> f Word) -> (Task -> f Task)
finalizationWait k x =
    fmap (\y -> x { _finalizationWait = y }) (k (_finalizationWait x))

-- | Options for an aurora job
data Job = Job
    { _task                   :: Task
    -- ^ The Task object to bind to this job
    , _jobName                :: Maybe String
    -- ^ Job name
    , _role                   :: String
    -- ^ Job role account
    , _cluster                :: String
    -- ^ Cluster in which this job is scheduled
    , _environment            :: Environment
    -- ^ Job environment
    , _contact                :: String
    -- ^ Best email address to reach the owner of the job.  For production jobs,
    --   this is usually a team mailing list
    , _instances              :: Word
    -- ^ Number of instances (sometimes referred to as replicas or shards) of
    --   the task to create
    , _updateConfig           :: UpdateConfig
    -- ^ Parameters for controlling the rate and policy of rolling updates
    , _jobConstraints         :: Map String String
    -- ^ Scheduling constraints for the tasks
    , _jobType                :: JobType
    -- ^ Specify whether the job is a service or cron job
    , _jobPermissibleFailures :: Maximum Word
    -- ^ Maximum permissible task failures
    , _priority               :: Integer
    -- ^ Preemption priority to give the task.  Tasks with higher priorities may
    --   preempt tasks at lower priorities
    , _production             :: Bool
    -- ^ Whether or not this is a production task backed by quota.  Production
    --   jobs may preempt any non-production job, and may only be preempted by
    --   production jobs in the same role 
    , _healthCheckConfig      :: HealthCheckConfig
    -- ^ Parameters for controlling a task’s health checks via HTTP. Only used
    --   if a health port was assigned with a command line wildcard
    } deriving (Eq, Show)

{-| Default `Job`

    Required fields: `task`, `role`, `cluster`, and `contact`

> _Job = Job
>     { _jobName                = Nothing
>     , _environment            = Devel
>     , _instances              = 1
>     , _updateConfig           = _UpdateConfig
>     , _jobConstraints         = Map.empty
>     , _jobType                = Ordinary
>     , _jobPermissibleFailures = 0
>     , _priority               = 0 
>     , _production             = False
>     , _healthCheckConfig      = _HealthCheckConfig
>     }
-}
_Job :: Job
_Job = Job
    { _jobName                = Nothing
    , _environment            = Devel
    , _instances              = 1
    , _updateConfig           = _UpdateConfig
    , _jobConstraints         = Map.empty
    , _jobType                = Ordinary
    , _jobPermissibleFailures = 0
    , _priority               = 0 
    , _production             = False
    , _healthCheckConfig      = _HealthCheckConfig
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
    task'                = prettyTask           (_task         j)
    name'                = qString (case _jobName j <|> _taskName (_task j) of
        Nothing  -> _processName (_process (_task j))
        Just str -> str )
    role'                = qString                 (_role              j)
    cluster'             = qString                 (_cluster           j)
    environment'         = prettyEnvironment       (_environment       j)
    contact'             = qString                 (_contact           j)
    instances'           = word                    (_instances         j)
    cronSchedule'        = case _jobType j of
        Cron s _ -> prettySchedule s
        _        -> text "None"
    cronCollisionPolicy' = prettyCollisionPolicy (case _jobType j of
        Cron _ c -> c
        _        -> KillExisting )
    updateConfig'        = prettyUpdateConfig (_updateConfig j)
    constraints'         = dict (map format (Map.assocs (_jobConstraints j)))
      where
        dict = encloseSep lbrace rbrace comma
        format (key, value) = text key <+> colon <+> text value
    service'             = bool (case _jobType j of
        Service -> True
        _       -> False )
    maxTaskFailures'     = int (case _jobPermissibleFailures j of
        Unlimited -> -1
        Finite n  -> fromIntegral n + 1 )
    priority'            = integer                 (_priority          j)
    production'          = bool                    (_production        j)
    healthCheckConfig'   = prettyHealthCheckConfig (_healthCheckConfig j)

{-|
> task :: Lens' Job Task
-}
task :: Functor f => (Task -> f Task) -> (Job -> f Job)
task k x = fmap (\y -> x { _task = y }) (k (_task x))

{-|
> jobName :: Lens' Job (Maybe String)
-}
jobName :: Functor f => (Maybe String -> f (Maybe String)) -> (Job -> f Job)
jobName k x = fmap (\y -> x { _jobName = y }) (k (_jobName x))

{-|
> role :: Lens' Job String
-}
role :: Functor f => (String -> f String) -> (Job -> f Job)
role k x = fmap (\y -> x { _role = y }) (k (_role x))

{-|
> cluster :: Lens' Job String
-}
cluster :: Functor f => (String -> f String) -> (Job -> f Job)
cluster k x = fmap (\y -> x { _cluster = y }) (k (_cluster x))

{-|
> environment :: Lens' Job Environment
-}
environment :: Functor f => (Environment -> f Environment) -> (Job -> f Job)
environment k x = fmap (\y -> x { _environment = y }) (k (_environment x))

{-|
> contact :: Lens' Job String
-}
contact :: Functor f => (String -> f String) -> (Job -> f Job)
contact k x = fmap (\y -> x { _contact = y }) (k (_contact x))

{-|
> instances :: Lens' Job Word
-}
instances :: Functor f => (Word -> f Word) -> (Job -> f Job)
instances k x = fmap (\y -> x { _instances = y }) (k (_instances x))

{-|
> updateConfig :: Lens' Job UpdateConfig
-}
updateConfig :: Functor f => (UpdateConfig -> f UpdateConfig) -> (Job -> f Job)
updateConfig k x = fmap (\y -> x { _updateConfig = y }) (k (_updateConfig x))

{-|
> jobConstraints :: Lens' Job (Map String String)
-}
jobConstraints
    :: Functor f
    => (Map String String -> f (Map String String)) -> (Job -> f Job)
jobConstraints k x =
    fmap (\y -> x { _jobConstraints = y }) (k (_jobConstraints x))

{-|
> jobType :: Lens' Job UpdateConfig
-}
jobType :: Functor f => (JobType -> f JobType) -> (Job -> f Job)
jobType k x = fmap (\y -> x { _jobType = y }) (k (_jobType x))

{-|
> jobPermissibleFailures :: Lens' Job (Maximum Word)
-}
jobPermissibleFailures
    :: Functor f => (Maximum Word -> f (Maximum Word)) -> (Job -> f Job)
jobPermissibleFailures k x =
    fmap (\y -> x { _jobPermissibleFailures = y })
         (k (_jobPermissibleFailures x))

{-|
> priority :: Lens' Job Integer
-}
priority :: Functor f => (Integer -> f Integer) -> (Job -> f Job)
priority k x = fmap (\y -> x { _priority = y }) (k (_priority x))

{-|
> production :: Lens' Job Bool
-}
production :: Functor f => (Bool -> f Bool) -> (Job -> f Job)
production k x = fmap (\y -> x { _production = y }) (k (_production x))

{-|
> healthCheckConfig :: Lens' Job HealthCheckConfig
-}
healthCheckConfig
    :: Functor f => (HealthCheckConfig -> f HealthCheckConfig) -> (Job -> f Job)
healthCheckConfig k x =
    fmap (\y -> x { _healthCheckConfig = y }) (k (_healthCheckConfig x))

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

{-|
> _Ordinary :: Traversal' JobType ()
-}
_Ordinary :: Applicative f => (() -> f ()) -> (JobType -> f JobType)
_Ordinary k x = case x of
    Ordinary -> fmap (\() -> Ordinary) (k ())
    _       -> pure x

{-|
> _Service :: Traversal' JobType ()
-}
_Service :: Applicative f => (() -> f ()) -> (JobType -> f JobType)
_Service k x = case x of
    Service -> fmap (\() -> Service) (k ())
    _       -> pure x

{-|
> _Cron :: Traversal' JobType (Schedule, CollisionPolicy)
-}
_Cron
    :: Applicative f
    => ((Schedule, CollisionPolicy) -> f (Schedule, CollisionPolicy))
    -> JobType -> f JobType
_Cron k x = case x of
    Cron a b -> fmap (\(a', b') -> Cron a' b') (k (a, b))
    _        -> pure x

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

{-|
> _KillExisting :: Traversal' CollisionPolicy ()
-}
_KillExisting
    :: Applicative f => (() -> f ()) -> (CollisionPolicy -> f CollisionPolicy)
_KillExisting k x = case x of
    KillExisting -> fmap (\() -> KillExisting) (k ())
    _            -> pure x

{-|
> _CancelNew :: Traversal' CollisionPolicy ()
-}
_CancelNew
    :: Applicative f => (() -> f ()) -> (CollisionPolicy -> f CollisionPolicy)
_CancelNew k x = case x of
    CancelNew -> fmap (\() -> CancelNew) (k ())
    _         -> pure x

{-| A `Cron` schedule

    Some example translations

> Schedule (Every 15) All All All All          -- */15 * * * *
> Schedule 59 23 31 December Friday            -- 59 23 31 12 5
> Schedule All All All (At [January,May]) All  -- * * * 1,5 *
> Schedule All All All (Range January May) All -- * * * 1-5 *
-}
data Schedule = Schedule
    { _minutes  :: Period Word8
    , _hours    :: Period Word8
    , _days     :: Period Word8
    , _months   :: Period Month
    , _weekdays :: Period Weekday
    } deriving (Eq, Show)

{-| Default `Schedule`

_Schedule = Schedule
    { _minutes  = All
    , _hours    = All
    , _days     = All
    , _months   = All
    , _weekdays = All
    }
-}
_Schedule :: Schedule
_Schedule = Schedule
    { _minutes  = All
    , _hours    = All
    , _days     = All
    , _months   = All
    , _weekdays = All
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

    minutes'  = fieldWith _minutes
    hours'    = fieldWith _hours
    days'     = fieldWith _days
    months'   = fieldWith (fmap (fromIntegral . succ . fromEnum) . _months  )
    weekdays' = fieldWith (fmap (fromIntegral . succ . fromEnum) . _weekdays)

{-|
> minutes :: Lens' Schedule (Period Word8)
-}
minutes
    :: Functor f
    => (Period Word8 -> f (Period Word8)) -> (Schedule -> f Schedule)
minutes k x = fmap (\y -> x { _minutes = y }) (k (_minutes x))

{-|
> hours :: Lens' Schedule (Period Word8)
-}
hours
    :: Functor f
    => (Period Word8 -> f (Period Word8)) -> (Schedule -> f Schedule)
hours k x = fmap (\y -> x { _hours = y }) (k (_hours x))

{-|
> days :: Lens' Schedule (Period Word8)
-}
days
    :: Functor f
    => (Period Word8 -> f (Period Word8)) -> (Schedule -> f Schedule)
days k x = fmap (\y -> x { _days = y }) (k (_days x))

{-|
> months :: Lens' Schedule (Period Month)
-}
months
    :: Functor f
    => (Period Month -> f (Period Month)) -> (Schedule -> f Schedule)
months k x = fmap (\y -> x { _months = y }) (k (_months x))

{-|
> weekdays :: Lens' Schedule (Period Weekday)
-}
weekdays
    :: Functor f
    => (Period Weekday -> f (Period Weekday)) -> (Schedule -> f Schedule)
weekdays k x = fmap (\y -> x { _weekdays = y }) (k (_weekdays x))

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

{-|
> _All :: Traversal' (Period n) ()
-}
_All :: Applicative f => (() -> f ()) -> (Period n -> f (Period n))
_All k x = case x of
    All -> fmap (\() -> All) (k ())
    _   -> pure x

{-|
> _Every :: Traversal' (Period n) n
-}
_Every :: Applicative f => (n -> f n) -> (Period n -> f (Period n))
_Every k x = case x of
    Every n -> fmap Every (k n)
    _       -> pure x

{-|
> _At :: Traversal' (Period n) [n]
-}
_At :: Applicative f => ([n] -> f [n]) -> (Period n -> f (Period n))
_At k x = case x of
    At n -> fmap At (k n)
    _    -> pure x

{-|
> _Range :: Traversal' (Period n) (n, n)
-}
_Range :: Applicative f => ((n, n) -> f (n, n)) -> (Period n -> f (Period n))
_Range k x = case x of
    Range n1 n2 -> fmap (\(n1', n2') -> Range n1' n2') (k (n1, n2))
    _           -> pure x

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

{-|
> _Sunday :: Traversal' Weekday ()
-}
_Sunday :: Applicative f => (() -> f ()) -> (Weekday -> f Weekday)
_Sunday k x = case x of
    Sunday -> fmap (\() -> Sunday) (k ())
    _      -> pure x

{-|
> _Monday :: Traversal' Weekday ()
-}
_Monday :: Applicative f => (() -> f ()) -> (Weekday -> f Weekday)
_Monday k x = case x of
    Monday -> fmap (\() -> Monday) (k ())
    _      -> pure x

{-|
> _Tuesday :: Traversal' Weekday ()
-}
_Tuesday :: Applicative f => (() -> f ()) -> (Weekday -> f Weekday)
_Tuesday k x = case x of
    Tuesday -> fmap (\() -> Tuesday) (k ())
    _       -> pure x

{-|
> _Wednesday :: Traversal' Weekday ()
-}
_Wednesday :: Applicative f => (() -> f ()) -> (Weekday -> f Weekday)
_Wednesday k x = case x of
    Wednesday -> fmap (\() -> Wednesday) (k ())
    _         -> pure x

{-|
> _Thursday :: Traversal' Weekday ()
-}
_Thursday :: Applicative f => (() -> f ()) -> (Weekday -> f Weekday)
_Thursday k x = case x of
    Thursday -> fmap (\() -> Thursday) (k ())
    _      -> pure x

{-|
> _Friday :: Traversal' Weekday ()
-}
_Friday :: Applicative f => (() -> f ()) -> (Weekday -> f Weekday)
_Friday k x = case x of
    Friday -> fmap (\() -> Friday) (k ())
    _      -> pure x

{-|
> _Saturday :: Traversal' Weekday ()
-}
_Saturday :: Applicative f => (() -> f ()) -> (Weekday -> f Weekday)
_Saturday k x = case x of
    Saturday -> fmap (\() -> Saturday) (k ())
    _        -> pure x

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

{-|
> _January :: Traversal' Month ()
-}
_January :: Applicative f => (() -> f ()) -> (Month -> f Month)
_January k x = case x of
    January -> fmap (\() -> January) (k ())
    _       -> pure x

{-|
> _February :: Traversal' Month ()
-}
_February :: Applicative f => (() -> f ()) -> (Month -> f Month)
_February k x = case x of
    February -> fmap (\() -> February) (k ())
    _        -> pure x

{-|
> _March :: Traversal' Month ()
-}
_March :: Applicative f => (() -> f ()) -> (Month -> f Month)
_March k x = case x of
    March -> fmap (\() -> March) (k ())
    _     -> pure x

{-|
> _April :: Traversal' Month ()
-}
_April :: Applicative f => (() -> f ()) -> (Month -> f Month)
_April k x = case x of
    April -> fmap (\() -> April) (k ())
    _     -> pure x

{-|
> _May :: Traversal' Month ()
-}
_May :: Applicative f => (() -> f ()) -> (Month -> f Month)
_May k x = case x of
    May -> fmap (\() -> May) (k ())
    _   -> pure x

{-|
> _June :: Traversal' Month ()
-}
_June :: Applicative f => (() -> f ()) -> (Month -> f Month)
_June k x = case x of
    June -> fmap (\() -> June) (k ())
    _    -> pure x

{-|
> _July :: Traversal' Month ()
-}
_July :: Applicative f => (() -> f ()) -> (Month -> f Month)
_July k x = case x of
    July -> fmap (\() -> July) (k ())
    _    -> pure x

{-|
> _August :: Traversal' Month ()
-}
_August :: Applicative f => (() -> f ()) -> (Month -> f Month)
_August k x = case x of
    August -> fmap (\() -> August) (k ())
    _      -> pure x

{-|
> _September :: Traversal' Month ()
-}
_September :: Applicative f => (() -> f ()) -> (Month -> f Month)
_September k x = case x of
    September -> fmap (\() -> September) (k ())
    _         -> pure x

{-|
> _October :: Traversal' Month ()
-}
_October :: Applicative f => (() -> f ()) -> (Month -> f Month)
_October k x = case x of
    October -> fmap (\() -> October) (k ())
    _       -> pure x

{-|
> _November :: Traversal' Month ()
-}
_November :: Applicative f => (() -> f ()) -> (Month -> f Month)
_November k x = case x of
    November -> fmap (\() -> November) (k ())
    _       -> pure x

{-|
> _December :: Traversal' Month ()
-}
_December :: Applicative f => (() -> f ()) -> (Month -> f Month)
_December k x = case x of
    December -> fmap (\() -> December) (k ())
    _       -> pure x

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

{-|
> _Devel :: Traversal' Environment ()
-}
_Devel :: Applicative f => (() -> f ()) -> (Environment -> f Environment)
_Devel k x = case x of
    Devel -> fmap (\() -> Devel) (k ())
    _     -> pure x

{-|
> _Test :: Traversal' Environment ()
-}
_Test :: Applicative f => (() -> f ()) -> (Environment -> f Environment)
_Test k x = case x of
    Test -> fmap (\() -> Test) (k ())
    _    -> pure x

{-|
> _Staging :: Traversal' Environment Int
-}
_Staging :: Applicative f => (Int -> f Int) -> (Environment -> f Environment)
_Staging k x = case x of
    Staging n -> fmap Staging (k n)
    _         -> pure x

{-|
> _Prod :: Traversal' Environment ()
-}
_Prod :: Applicative f => (() -> f ()) -> (Environment -> f Environment)
_Prod k x = case x of
    Prod -> fmap (\() -> Prod) (k ())
    _    -> pure x

-- | Parameters for controlling the rate and policy of rolling updates
data UpdateConfig = UpdateConfig
    { _batchSize                    :: Word
    -- ^ Maximum number of shards to be updated in one iteration
    , _restartThreshold             :: Word
    -- ^ Maximum number of seconds before a shard must move into the `RUNNING`
    --   state before considered a failure 
    , _watchSecs                    :: Word
    -- ^ Minimum number of seconds a shard must remain in `RUNNING` state before
    --   considered a success 
    , _perShardPermissibleFailures  :: Word
    -- ^ Maximum number of permissible failures during update. Increments total
    --   failure count when this limit is exceeded
    , _totalPermissibleFailures     :: Word
    -- ^ Maximum number of shard failures to be tolerated in total during an
    --   update. Cannot be greater than or equal to the total number of tasks
    --   in a job
    } deriving (Eq, Show)

{-| Default `UpdateConfig`

> _UpdateConfig :: UpdateConfig
> _UpdateConfig = UpdateConfig
>     { _batchSize                   = 1
>     , _restartThreshold            = 60
>     , _watchSecs                   = 45
>     , _perShardPermissibleFailures = 0
>     , _totalPermissibleFailures    = 0
>     }
-}
_UpdateConfig :: UpdateConfig
_UpdateConfig = UpdateConfig
    { _batchSize                   = 1
    , _restartThreshold            = 60
    , _watchSecs                   = 45
    , _perShardPermissibleFailures = 0
    , _totalPermissibleFailures    = 0
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
    batchSize'           = word (_batchSize                   u)
    restartThreshold'    = word (_restartThreshold            u)
    watchSecs'           = word (_watchSecs                   u)
    maxPerShardFailures' = word (_perShardPermissibleFailures u)
    maxTotalFailures'    = word (_totalPermissibleFailures    u)

{-|
> batchSize :: Lens' UpdateConfig Word
-}
batchSize :: Functor f => (Word -> f Word) -> (UpdateConfig -> f UpdateConfig)
batchSize k x = fmap (\y -> x { _batchSize = y }) (k (_batchSize x))

{-|
> restartThreshold :: Lens' UpdateConfig Word
-}
restartThreshold
    :: Functor f => (Word -> f Word) -> (UpdateConfig -> f UpdateConfig)
restartThreshold k x =
    fmap (\y -> x { _restartThreshold = y }) (k (_restartThreshold x))

{-|
> watchSecs :: Lens' UpdateConfig Word
-}
watchSecs :: Functor f => (Word -> f Word) -> (UpdateConfig -> f UpdateConfig)
watchSecs k x = fmap (\y -> x { _watchSecs = y }) (k (_watchSecs x))

{-|
> perShardPermissibleFailures :: Lens' UpdateConfig Word
-}
perShardPermissibleFailures
    :: Functor f => (Word -> f Word) -> (UpdateConfig -> f UpdateConfig)
perShardPermissibleFailures k x =
    fmap (\y -> x { _perShardPermissibleFailures = y })
         (k (_perShardPermissibleFailures x))

{-|
> totalPermissibleFailures :: Lens' UpdateConfig Word
-}
totalPermissibleFailures
    :: Functor f => (Word -> f Word) -> (UpdateConfig -> f UpdateConfig)
totalPermissibleFailures k x =
    fmap (\y -> x { _totalPermissibleFailures = y })
         (k (_totalPermissibleFailures x))

-- | Parameters for controlling a task’s health checks via HTTP.
data HealthCheckConfig = HealthCheckConfig
    { _initialIntervalSecs            :: Word
    -- ^ Initial delay for performing an HTTP health check
    , _intervalSecs                   :: Word
    -- ^ Interval on which to check the task’s health via HTTP
    , _timeoutSecs                    :: Word
    -- ^ HTTP request timeout
    , _consecutivePermissibleFailures :: Word
    -- ^ Consecutive failures tolerated before considering a task unhealthy
    } deriving (Eq, Show)

{-| Default `HealthCheckConfig`
-}
_HealthCheckConfig :: HealthCheckConfig
_HealthCheckConfig = HealthCheckConfig
    { _initialIntervalSecs            = 15
    , _intervalSecs                   = 10
    , _timeoutSecs                    = 1
    , _consecutivePermissibleFailures = 0
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
    initialIntervalSecs'    = word (_initialIntervalSecs            h)
    intervalSecs'           = word (_intervalSecs                   h)
    timeoutSecs'            = word (_timeoutSecs                    h)
    maxConsecutiveFailures' = word (_consecutivePermissibleFailures h)

{-|
> initialIntervalSecs :: Lens' HealthCheckConfig Word
-}
initialIntervalSecs
    :: Functor f
    => (Word -> f Word) -> (HealthCheckConfig -> f HealthCheckConfig)
initialIntervalSecs k x =
    fmap (\y -> x { _initialIntervalSecs = y }) (k (_initialIntervalSecs x))

{-|
> intervalSecs :: Lens' HealthCheckConfig Word
-}
intervalSecs
    :: Functor f
    => (Word -> f Word) -> (HealthCheckConfig -> f HealthCheckConfig)
intervalSecs k x = fmap (\y -> x { _intervalSecs = y }) (k (_intervalSecs x))

{-|
> timeoutSecs :: Lens' HealthCheckConfig Word
-}
timeoutSecs
    :: Functor f
    => (Word -> f Word) -> (HealthCheckConfig -> f HealthCheckConfig)
timeoutSecs k x = fmap (\y -> x { _timeoutSecs = y }) (k (_timeoutSecs x))

{-|
> consecutivePermissibleFailures :: Lens' HealthCheckConfig Word
-}
consecutivePermissibleFailures
    :: Functor f
    => (Word -> f Word) -> (HealthCheckConfig -> f HealthCheckConfig)
consecutivePermissibleFailures k x =
    fmap (\y -> x { _consecutivePermissibleFailures = y })
         (k (_consecutivePermissibleFailures x))

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

{-|
> _Unlimited :: Traversal' (Maximum a) ()
-}
_Unlimited :: Applicative f => (() -> f ()) -> (Maximum a -> f (Maximum a))
_Unlimited k x = case x of
    Unlimited -> fmap (\() -> Unlimited) (k ())
    _         -> pure x

{-|
> _Finite :: Traversal' (Maximum a) a
-}
_Finite :: Applicative f => (a -> f a) -> (Maximum a -> f (Maximum a))
_Finite k x = case x of
    Finite a -> fmap Finite (k a)
    _        -> pure x

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
