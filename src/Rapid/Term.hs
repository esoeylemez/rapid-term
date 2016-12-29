-- |
-- Copyright:  (c) 2016 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>

module Rapid.Term
    ( -- * Terminal support for Rapid
      Term,
      newTermRef,
      runTerm,
      termFd,
      terminal,
      -- ** Low-level
      termFdPure,
      terminalPure,
      waitTerm,
      withTerm,

      -- * Supported terminal emulators
      -- ** rxvt-unicode
      urxvt,
      urxvtc,
      urxvtAt
    )
    where

import Control.Concurrent
import Control.Exception
import Control.Monad.Codensity
import Control.Monad.IO.Class
import Data.IORef
import System.IO
import System.Mem.Weak
import System.Posix.IO
import System.Posix.Terminal
import System.Posix.Types
import System.Process


-- | Handle to a terminal

data Term =
    Term {
      _process  :: ProcessHandle,  -- ^ Process handle
      _ttySlave :: Fd              -- ^ File descriptor
    }


-- | Create a new terminal reference.

newTermRef :: IO (MVar Term)
newTermRef = newEmptyMVar


-- | Start a terminal and update the given terminal reference for use
-- from other threads.

runTerm :: (Fd -> IO ProcessHandle) -> MVar Term -> IO ()
runTerm start var =
    withTerm start $ \t ->
        mask $ \unmask ->
            bracket_ (unmask (putMVar var t))
                     (takeMVar var)
                     (unmask (waitTerm t))


-- | Provide a file descriptor to the given terminal
--
-- Given a terminal, this function duplicates its file descriptor and
-- passes it to the given continuation.  It is closed after the
-- continuation returns.
--
-- If you need separate file descriptors for input and output, you can
-- cascade this function in the same way as 'terminal'.
--
-- You can use this function as often as you want, in sequence or
-- concurrently.

termFd :: MVar Term -> (Fd -> IO r) -> IO r
termFd tRef k = readMVar tRef >>= \t -> termFdPure t k


-- | Variant of 'termFd' that works on a pure terminal handle

termFdPure :: Term -> (Fd -> IO r) -> IO r
termFdPure t = bracket (dup (_ttySlave t)) closeFd


-- | Provide a handle to the given terminal
--
-- Given a terminal, this function creates a handle (by duplicating the
-- underlying file descriptor) and passes it to the given continuation.
-- It is closed after the continuation returns.
--
-- If you need separate handles for input and output (for example to
-- select different buffering modes), just cascade this function:
--
-- > terminal t (\hI -> terminal t (\hO -> k hI hO))
--
-- You can use this function as often as you want, in sequence or
-- concurrently.

terminal :: MVar Term -> (Handle -> IO r) -> IO r
terminal tRef k = readMVar tRef >>= \t -> terminalPure t k


-- | Variant of 'terminal' that works on a pure terminal handle

terminalPure :: Term -> (Handle -> IO r) -> IO r
terminalPure t k =
    mask $ \unmask ->
        let mkTtyHandle = unmask (dup (_ttySlave t)) >>= fdToHandle
        in bracket mkTtyHandle hClose $ \h ->
               unmask $ do
                   hSetBinaryMode h False
                   hSetBuffering h LineBuffering
                   hSetEcho h True
                   hSetEncoding h localeEncoding
                   hSetNewlineMode h nativeNewlineMode
                   k h


-- | Spawns rxvt-unicode using the @urxvt@ executable

urxvt :: Fd -> IO ProcessHandle
urxvt = urxvtAt "urxvt"


-- | Spawns rxvt-unicode using the @urxvtc@ executable

urxvtc :: Fd -> IO ProcessHandle
urxvtc = urxvtAt "urxvtc"


-- | Spawns rxvt-unicode using the given executable

urxvtAt :: FilePath -> Fd -> IO ProcessHandle
urxvtAt p fd = spawnProcess p ["-pty-fd", show fd]


-- | Wait for the given terminal subprocess to exit

waitTerm :: Term -> IO ()
waitTerm = (() <$) . waitForProcess . _process


-- | Create a terminal using the given spawn function and pass its
-- terminal handle to the given continuation
--
-- The subprocess is terminated and resources are cleaned up once the
-- continuation returns.

withTerm
    :: (Fd -> IO ProcessHandle)  -- ^ Spawn function
    -> (Term -> IO r)            -- ^ Continuation
    -> IO r
withTerm start k =
    mask $ \unmask -> lowerCodensity $ do
        (master, slave) <- liftIO (unmask openPseudoTerminal)

        masterRef <- liftIO (newIORef master)
        masterWeak <- liftIO (mkWeakIORef masterRef (closeFd master))
        cOnException (finalize masterWeak)
        cFinally (closeFd slave)

        liftIO (unmask (setFdOption master CloseOnExec False))

        ph <- cBracket (unmask (start master))
                       (\ph -> unmask (terminateProcess ph >> waitForProcess ph))
        liftIO (unmask (finalize masterWeak))

        liftIO (unmask (k (Term {
                             _process  = ph,
                             _ttySlave = slave
                           })))

    where
    cBracket :: IO a -> (a -> IO b) -> Codensity IO a
    cBracket c o = Codensity (bracket c o)

    cFinally :: IO a -> Codensity IO ()
    cFinally c = Codensity (\k -> k () `finally` c)

    cOnException :: IO a -> Codensity IO ()
    cOnException c = Codensity (\k -> k () `onException` c)
