{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}

-- | This library provides process spawning facilities for POSIX systems.
--
-- = Features
--
--  * Always reaps children spawned by this library, no need to explicitly
--    wait for each child if you don't care about the result.
--  * Uses @ByteString@s for OS strings.
--  * Works with the non-threaded and threaded runtimes.
--  * Uses @SIGCHILD@ to detect child process termination.
--  * Can connect pipes at specifc file descriptors.
--  * Can connect pipes to free file descriptors and communicate the file
--    descriptor to the child process using environment variables.
--  * Signals are always sent to the intended process, even if your system
--    reuses PIDs.
--  * Types help you to ensure you feed and read all pipes.
--  * Pipes are guaranteed to only go to their destined process, regardless
--    of spawning processes in parallel (as long as you only fork processes
--    with this library).
--
-- Currently it only works on POSIX systems.
--
-- NOTE: This library modifies your @SIGCHILD@ signal handler. It will
-- not work if you reset this.
--
-- NOTE: If you use the @fork()@ system call, we do not guarantee that file
-- descriptors created by this library won't leak. In fact, we know of no way
-- to guarantee this in a threaded program across all systems. If the only
-- @fork()@ing you do is from this library, then we do ensure that file
-- descriptors don't leak.
--
-- NOTE: If you create file descriptors in other parts of your program, you
-- should bracket their creation inside the @'dontFork'@ function and make sure
-- that you have set the `CLOEXEC` flag before they return to prevent them
-- leaking into process forked by this library.
--
-- = Examples
--
-- Simple process execution, feeding stdin, and reading stdout.
--
-- >>> captureInOut (proc "md5sum" []) "Hello"
-- ("8b1a9953c4611296a827abf8c47804d7  -\n",Exited ExitSuccess)
--
-- >>> captureInOutErr (proc "cat" ["-", "nofile"]) "Hello, world!"
-- (StdOutputs {getStdOut = "Hello, world!", getStdErr = "cat: nofile: No such file or directory\n"},Exited (ExitFailure 1))

module System.Process
    ( ProcessSpec(..)
    , ProcessHandle()
    , PipeLocation(..)
    , proc
    , procWith
    -- | == Adding pipes
    -- These take a @'ProcessSpec'@ and add additional pipes to it. You can
    -- instead use @'procWith'@ to create all the pipes you want at once.
    , readPipe
    , readPipes
    , writePipe
    , writePipes
    -- | == Running a process
    , spawn
    , capture
    , captureIn
    , captureOut
    , captureInOut
    , captureInOutErr
    , handlePipes
    , handleIn
    , handleOut
    , handleInOut
    -- | == Interacting with processes
    , processWait
    , processTryWait
    , processSignal
    -- | == Standard locations
    -- Use these when constructing your pipes if you need to refer to
    -- standard locations.
    , stdOut
    , stdErr
    , stdIn
    , stdOutputs
    , StdOutputs(..)
    -- | == Composable types for adding pipes.
    , Pipes(..)
    , NoPipe(..)
    , (:|)(..)
    , AddPipe
    -- | == Miscellaneous
    , dontFork
    ) where

import Data.String
import GHC.Generics
import Control.DeepSeq
import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Char8 as Char8
import Control.Concurrent.Async
import qualified Data.ByteString.Lazy as Lazy
import System.IO (Handle, hClose)
import Control.Concurrent.MVar
import Control.Concurrent (threadDelay)
import Control.Monad
import System.Posix.ByteString
import System.IO.Unsafe (unsafePerformIO)
import Data.Functor
import Data.Functor.Const
import Data.Functor.Product
import Data.Functor.Identity
import Data.Functor.Classes
import Data.Unique

{-# NOINLINE childrenVar #-}
childrenVar :: MVar [((ProcessID, Unique), MVar ProcessStatus)]
childrenVar = unsafePerformIO newEmptyMVar

{-# NOINLINE initialised #-}
initialised :: MVar ()
initialised = unsafePerformIO newEmptyMVar

-- | This is the sigCHLD handler. You should never need to call this manually.
reap :: IO ()
reap = do
    modifyMVar_ childrenVar $ filterM $
        \((pid, _), statusVar) -> do
            getProcessStatus False True pid >>= \case
                Nothing     -> pure True
                Just status -> do
                    putMVar statusVar status
                    pure False

initialise :: IO ()
initialise = tryPutMVar initialised () >>= \case
    True -> do
        installHandler sigCHLD (Catch reap) Nothing
        putMVar childrenVar []
    False -> pure ()

-- | This type represents the description of a process that is going to be
-- executed.
data ProcessSpec ipipes opipes = ProcessSpec
    { command     :: !ByteString             -- ^ The command to run
    , arguments   :: ![ByteString]           -- ^ The positional arguments
    , searchPath  :: !Bool                   -- ^ Should we search your PATH
                                             --   environment variable?
    , inputs      :: !(ipipes PipeLocation)  -- ^ The input pipes to create.
                                             --   The child will read these.
    , outputs      :: !(opipes PipeLocation) -- ^ The output pipes to create.
                                             --   The child will write these.
    }

deriving instance (Show (i PipeLocation), Show (o PipeLocation)) => Show (ProcessSpec i o)

-- | Create a pipe that the child can write to.
writePipe :: PipeLocation -> ProcessSpec i o -> ProcessSpec i (AddPipe o)
writePipe loc ProcessSpec{..} = ProcessSpec
    { outputs = outputs :| loc
    , ..
    }

-- | Create a pipe that the child can read from.
readPipe :: PipeLocation -> ProcessSpec i o -> ProcessSpec (AddPipe i) o
readPipe loc ProcessSpec{..} = ProcessSpec
    { inputs = inputs :| loc
    , ..
    }

-- | Create potentially multiple pipes that the child can write to.
writePipes :: o PipeLocation -> ProcessSpec i o' -> ProcessSpec i (Product o' o)
writePipes locs ProcessSpec{..} = ProcessSpec
    { outputs = Pair outputs locs
    , ..
    }

-- | Create potentially multiple pipes that the child can read from.
readPipes :: i PipeLocation -> ProcessSpec i' o -> ProcessSpec (Product i' i) o
readPipes locs ProcessSpec{..} = ProcessSpec
    { inputs = Pair inputs locs
    , ..
    }

-- | This type represents a running process. Use this to send signals, or
-- wait for the process to finish.
--
-- See @'processWait'@, @'processTryWait'@ and @'processSignal'@.
data ProcessHandle = ProcessHandle ProcessID Unique (MVar ProcessStatus)

-- | Wait for this process to exit, and return it's exit status.
--
-- You do NOT have to call this in order to clean up your process. It will
-- be reaped regardless.
--
-- Note: this call will not return until the process has exited, but it does
-- NOT block the thread, and is safe to use in the single threaded runtime.
processWait :: ProcessHandle -> IO ProcessStatus
processWait (ProcessHandle _ _ var) = readMVar var

-- | Like @'processWait'@, but does not block.
processTryWait :: ProcessHandle -> IO (Maybe ProcessStatus)
processTryWait (ProcessHandle _ _ var) = tryReadMVar var

-- | A type to use when you don't want any pipes. Also used as the "@Nil@"
-- element when you add pipes using @'AddPipe'@.
--
-- The @'proc'@ command will create a @'ProcessSpec' 'NoPipe' 'NoPipe'@ to
-- signal that by default there are no pipes created. You can then use some
-- of the combinators in this library to add new pipes, or you can bypass
-- the @'proc'@ call and use @'procWith'@ instead to directly specify the
-- pipes you want to create.
data NoPipe a = NoPipe
    deriving stock (Functor, Foldable, Traversable, Show)

-- | Add another pipe onto a process. (See also the @'AddPipe'@ alias which
-- is easier to read in some scenarios.)
data b :| a = (b a) :| a
    deriving stock (Functor, Foldable, Traversable, Show)

-- | Sometimes this is easier to read.
type AddPipe = (:|)

instance Show1 NoPipe where
    liftShowsPrec f l i n x = "NoPipe" ++ x

instance Show1 b => Show1 ((:|) b) where
    liftShowsPrec f l i (b :| x) s = "APipe " ++ liftShowsPrec f l i b "" ++ f i x s

instance IsString s => IsString (NoPipe :| s) where
    fromString s = NoPipe :| (fromString s)


-- | Create a @'ProcessSpec'@ with the given pipes.
--
-- >>> capture (procWith NoPipe stdOutputs "echo" ["Hello, world!"]) NoPipe
-- (StdOuputs { getStdOut = "Hello, world!\n", getStdErr = ""},Exited ExitSuccess)
--
-- These can still be extended with the @'readPipe'@, @'writePipe'@, etc.
-- functions.
procWith :: i PipeLocation -> o PipeLocation -> ByteString -> [ByteString] -> ProcessSpec i o
procWith i o cmd args = ProcessSpec
    { command    = cmd
    , arguments  = args
    , searchPath = True
    , inputs     = i
    , outputs    = o
    }

-- | Create a @'ProcessSpec'@ with no pipes. These can later be extended
-- with various functions in this library. e.g. @'readPipe'@, @'writePipe'@, etc.
proc :: ByteString -> [ByteString] -> ProcessSpec NoPipe NoPipe
proc = procWith NoPipe NoPipe

-- | Send a @'Signal'@ to this process. The signal is guaranteed to be sent
-- to exactly this process even if your system reuses process ID's.
processSignal :: ProcessHandle -> Signal -> IO ()
processSignal (ProcessHandle pid ident var) sig = do
    withMVar childrenVar $ \children -> do
        if (pid, ident) `elem` map fst children
        then signalProcess sig pid
        else pure () -- process already finished.

-- Traversable with Finally.
-- withSpawn :: ProcessSpec i o -> (ProcessHandle -> i Fd -> o Fd -> IO a) -> IO a
-- withSpawn p f = bracket (spawn p) cleanup (\(a,b,c) -> f a b c)
--     where
--         cleanup (h,i,o) = do
--             traverse closeFd i

-- | This is the most primite spawning operation in this library. It takes
-- a @'ProcessSpec'@ and turns it into a process. You can interact with the
-- process using the returned @'ProcessHandle'@ and input and output file
-- descriptors.
--
-- There is no need to call `processWait` unless you want to know the exit
-- code and reason of the process. It will be reaped regardless. You probably
-- do want to call @'closeFd'@ on the file descriptors though.
spawn :: (Traversable i, Traversable o) => ProcessSpec i o -> IO (ProcessHandle, i Fd, o Fd)
spawn ProcessSpec{..} = do
    -- The ProcessSpec data gets shipped to the new process, so
    -- we want to make sure it's all there incase there is any
    -- state waiting to explode.

    -- Always do this so we don't have to have an explicit setup.
    -- It should be cheap (it's checking one MVar on the fast path).
    initialise
    modifyMVar childrenVar $ \children -> do
        -- NOTE: `seq` is important here
        r <- traverse (\x -> x `seq` (x,) <$> createPipe) inputs
        w <- traverse (\x -> x `seq` (x,) <$> createPipe) outputs

        evaluate (force (command, arguments, searchPath))

        pid <- forkProcess $ do
            traverse (\(x,(re,we)) -> do
                closeFd we
                case x of
                    Dup2 fd -> void $ dupTo re fd
                    Env  var -> setEnv var (Char8.pack (show re)) True
                ) r
            traverse (\(x,(re,we)) -> do
                closeFd re
                case x of
                    Dup2 fd -> void $ dupTo we fd
                    Env  var -> setEnv var (Char8.pack (show we)) True
                ) w
            executeFile command searchPath arguments Nothing

        -- We set CloseOnExec so that other spawned process can't capture
        -- these. On some systems the `pipe2()` system call would allow us to
        -- atomically create these, however not all systems support it (MacOS).
        --
        -- If you use this library as the only source of `forks`, then we are
        -- protected by the childrenMVar anyway. So there should be no issue.
        traverse (\(x,(re,we)) -> closeFd re >> setFdOption we CloseOnExec True) r
        traverse (\(x,(re,we)) -> closeFd we >> setFdOption re CloseOnExec True) w

        psVar <- newEmptyMVar
        u <- newUnique

        pure (((pid,u),psVar):children, (ProcessHandle pid u psVar, fmap (snd . snd) r, fmap (fst . snd) w))

-- | A class for containers that contain pipe locations.
--
-- This essentially just provides an aligned, uniform shaped `zip` function
-- which is used to align the provided pipe handlers with the created pipes.
class Traversable a => Pipes a where
    zipPipesM :: Monad m => (x -> y -> m z) -> a x -> a y -> m (a z)

instance Pipes NoPipe where
    zipPipesM _ _ _ = pure NoPipe

instance Pipes b => Pipes (AddPipe b) where
    zipPipesM f (b :| a) (b' :| a') = do
        r <- zipPipesM f b b'
        t <- f a a'
        pure (r :| t)

instance (Pipes a, Pipes b) => Pipes (Product a b) where
    zipPipesM f (Pair a b) (Pair a' b') = Pair <$> zipPipesM f a a' <*> zipPipesM f b b'

-- | Defines where to create a pipe.
--
-- For some pre-constructed values of this, see @'stdOut'@, @'stdErr'@,
-- @'stdIn'@ and @'stdOutputs'@
data PipeLocation
    = Dup2 !Fd            -- ^ Move the pipe to the specified file descriptor.
    | Env  !ByteString    -- ^ Find a free file descriptor, and launch the
                          --   process with the given environment variable
                          --   set to the file descriptor number.

zipPipes :: Pipes a => a x -> a y -> a (x,y)
zipPipes x y = runIdentity $  zipPipesM (\x y -> pure (x,y)) x y

-- | Send `sigTERM` to a process after the timeout. If it still doesn't die
-- send `sigKILL` after a further 1 second.
timeoutProcess :: Int -> ProcessHandle -> IO ProcessStatus
timeoutProcess i ph =
    either id id <$> race
        ( do
            threadDelay i
            processSignal ph sigTERM
            either id id <$> race
                (processWait ph)
                (do
                    threadDelay 1000000
                    processSignal ph sigKILL
                    processWait ph
                )
        )
        (processWait ph)


-- | A function that allows you to capture all the read pipes, and write
-- to all the write pipes.
--
-- >>> capture (readPipe stdIn $ writePipe stdOut $ proc "xxd" []) "Hello" processWait
-- (APipe NoPipe "00000000: 4865 6c6c 6f                             Hello\n",Exited ExitSuccess)
capture :: (Pipes i, Pipes o) => ProcessSpec i o -> i Lazy.ByteString -> IO (o Strict.ByteString, ProcessStatus)
capture p input = do
    (is,os,s) <- handlePipes p
        (fmap (\b -> \i -> Lazy.hPutStr i b) input)
        (fmap (\_ -> Strict.hGetContents) (outputs p))
        processWait
    pure (os,s)

-- | Spawn the process, and run an IO action on each handle created.
-- Note that this does not wait for the process to exit unless the one
-- of the handlers does so.
handlePipes :: (Pipes i, Pipes o) => ProcessSpec i o -> i (Handle -> IO ai) -> o (Handle -> IO ao) -> (ProcessHandle -> IO ap) -> IO (i ai, o ao, ap)
handlePipes p input output handle = do
    (ph, ins, outs) <- spawn p

    ihs <- traverse fdToHandle ins
    let ihsWithInput = zipPipes ihs input
    ohs <- traverse fdToHandle outs
    let ohsWithOutput = zipPipes ohs output

    ((os, pa), is) <- concurrently
        ( concurrently
            ( forConcurrently ohsWithOutput $ \(oh, o) -> o oh )
            ( handle ph )
        )
        ( forConcurrently ihsWithInput $ \(ih,i) -> do
            r <- i ih `finally` hClose ih
            pure r
        )
    pure (is, os, pa)

-- | Spawn a process, and write to stdin.
captureIn :: ProcessSpec NoPipe NoPipe -> Lazy.ByteString -> IO ProcessStatus
captureIn p i = snd <$> capture (readPipe stdIn p) (NoPipe :| i)

-- | Spawn a process and read from stdout.
captureOut :: ProcessSpec NoPipe NoPipe -> IO (Strict.ByteString, ProcessStatus)
captureOut p = do
    (NoPipe :| x, y) <- capture (writePipe stdOut p) NoPipe
    pure (x,y)

-- | Spawn a process and write to stdin and read from stdout.
captureInOut :: ProcessSpec NoPipe NoPipe -> Lazy.ByteString -> IO (Strict.ByteString, ProcessStatus)
captureInOut p i = do
    (NoPipe :| x, y) <- capture (writePipe stdOut $ readPipe stdIn p) (NoPipe :| i)
    pure (x,y)

-- | Spawn a process and write to stdin, and read from both stdout and stderr.
captureInOutErr :: ProcessSpec NoPipe NoPipe -> Lazy.ByteString -> IO (StdOutputs Strict.ByteString, ProcessStatus)
captureInOutErr p i = do
    (Pair NoPipe x, y) <- capture (writePipes stdOutputs $ readPipe stdIn p) (NoPipe :| i)
    pure (x,y)

-- | Run an IO with access to a @'Handle'@ connected to stdin.
handleIn :: ProcessSpec NoPipe NoPipe -> (Handle -> IO a) -> IO (a, ProcessStatus)
handleIn p i = (\(NoPipe :| a, NoPipe, p) -> (a, p)) <$> handlePipes (readPipe stdIn p) (NoPipe :| i) NoPipe processWait

-- | Run an IO with access to a @'Handle'@ connected to stdout.
handleOut :: ProcessSpec NoPipe NoPipe -> (Handle -> IO a) -> IO (a, ProcessStatus)
handleOut p o = (\(NoPipe, NoPipe :| a, p) -> (a, p)) <$> handlePipes (writePipe stdOut p) NoPipe (NoPipe :| o) processWait

-- | Run two IO actions concurrently each with a @'Handle'@, one connected to
-- stdin, the other to stdout.
handleInOut :: ProcessSpec NoPipe NoPipe -> (Handle -> IO a) -> (Handle -> IO b) -> IO (a,b,ProcessStatus)
handleInOut p i o = (\(NoPipe :| a, NoPipe :| b, p) -> (a,b,p)) <$> handlePipes (writePipe stdOut (readPipe stdIn p)) (NoPipe :| i) (NoPipe :| o) processWait


-- | Use @'stdOutputs'@ to construct this. Will create pipes for @stderr@
-- and @stdout@.
data StdOutputs a = StdOutputs
    { getStdOut :: a
    , getStdErr :: a
    } deriving (Functor, Foldable, Traversable)

instance Pipes StdOutputs where
    zipPipesM f a b = StdOutputs <$> f (getStdOut a) (getStdOut b) <*> f (getStdErr a) (getStdErr b)

instance Show1 StdOutputs where
    liftShowsPrec f l i (StdOutputs o e) r =
        "StdOuputs { getStdOut = " ++ f i o (
                  ", getStdErr = " ++ f i e (
                  "}" ++ r))

instance Show a => Show (StdOutputs a) where
    showsPrec = liftShowsPrec showsPrec showList

-- | A precomposed value of @'StdOutputs'@ which refers to stdout and stderr
-- and can be used directly when you want to capture those.
--
-- >>> capture (procWith NoPipe stdOutputs "echo" ["yes"]) NoPipe
-- (StdOuputs { getStdOut = "yes\n", getStdErr = ""},Exited ExitSuccess)
stdOutputs :: StdOutputs PipeLocation
stdOutputs = StdOutputs
    { getStdOut = stdOut
    , getStdErr = stdErr
    }

-- | Creates a pipe connected to the stdin of the child process.
stdIn :: PipeLocation
stdIn = Dup2 0

-- | Creates a pipe connected to the stdout of the child process.
stdOut :: PipeLocation
stdOut = Dup2 1

-- | Creates a pipe connected to the stderr of the child process.
stdErr :: PipeLocation
stdErr = Dup2 2

-- | This will run the given IO action while preventing this library from doing
-- almost everything. It won't fork, it won't create pipes, and it won't reap
-- children processes. IO actions should therefore remain very short. The
-- purpose of this is for when you need to create a file descriptor that can
-- not atomically have the @CLOEXEC@ bit set, and so you need to do it
-- non-atomically across multiple system calls.
--
-- The IO action MUST NOT call any other function from this library.
dontFork :: IO a -> IO a
dontFork a = withMVar childrenVar (\_ -> a)
