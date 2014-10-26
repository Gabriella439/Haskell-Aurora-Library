module Aurora (
    -- * Process
      Process(..)
    , _Process
    , processName
    , cmdline
    , processPermissibleFailures
    , daemon
    , ephemeral
    , minDuration
    , final

    -- * Constraint
    , Constraint(..)
    , order
    , order'

    -- * Resource
    , Resource(..)
    , cpu
    , ram
    , disk

    -- * Task
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

    -- * Job
    , Job(..)
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

    -- ** JobType
    , JobType(..)

    -- *** CollisionPolicy
    , CollisionPolicy(..)

    -- *** Cron
    , CronSchedule(..)

    -- **** Schedule
    , Schedule(..)

    -- **** Weekday
    , Weekday(..)

    -- **** Month
    , Month(..)

    -- ** Environment
    , Environment(..)

    -- ** UpdateConfig
    , UpdateConfig(..)

    -- ** HealthCheckConfig
    , HealthCheckConfig

    -- * Maximum
    , Maximum(..)

    -- * Re-exports
    , Word
    ) where

import Control.Applicative (Applicative(pure, (<*>)), liftA2)
import Data.Map (Map)
import Data.Word (Word, Word8)

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
    , _taskPermissibleFailures :: Maximum Word
    -- ^ Maximum permissible process failures
    , _maxConcurrency          :: Maximum Word
    -- ^ Maximum number of concurrent processes
    , _finalizationWait        :: Word
    -- ^ Amount of time allocated for finalizing processes, in seconds
    } deriving (Eq, Show)

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
taskPermissibleFailures
    :: Functor f => (Maximum Word -> f (Maximum Word)) -> (Task -> f Task)
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

-- | Specify whether the job is a service or cron job
data JobType
    = Service
    -- ^ Long-running service
    | Cron CronSchedule CollisionPolicy
    -- ^ Periodic command
    deriving (Eq, Show)

-- | How to handle existing cron jobs
data CollisionPolicy
    = KillExisting
    -- ^ Kill any existing jobs in progress
    | CancelNew
    -- ^ Let any existing jobs finish
    deriving (Eq, Show)

{-| A `Cron` schedule

    Some example translations

> Schedule (Every 15) All All All All          -- */15 * * * *
> Schedule 59 23 31 December Friday            -- 59 23 31 12 5
> Schedule All All All (At [January..May]) All -- * * * 1-5 *
-}
data CronSchedule = Schedule
    { _minutes  :: Schedule Word8
    , _hours    :: Schedule Word8
    , _days     :: Schedule Word8
    , _months   :: Schedule Month
    , _weekdays :: Schedule Weekday
    } deriving (Eq, Show)

-- | One field of a `CronSchedule`
data Schedule n
    = All
    -- ^ @*@
    | Every n
    -- ^ @*/n@
    | At [n]
    -- ^ @n1,n2,n3@
    deriving (Eq, Show)

instance Num n => Num (Schedule n) where
    fromInteger n = At [fromInteger n]

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

data UpdateConfig = UpdateConfig deriving (Eq, Show)

data HealthCheckConfig = HealthCheckConfig deriving (Eq, Show)

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
