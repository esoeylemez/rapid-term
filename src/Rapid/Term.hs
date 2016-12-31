-- |
-- Copyright:  (c) 2016 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>
--
-- When developing interactive command line applications in an editor
-- like Emacs GHCi typically has no access to an actual terminal.  This
-- is good enough for applications that only read lines from stdin and
-- print diagnostics to stdout, but as soon as terminal functionality is
-- needed, the application has to be tested elsewhere.
--
-- This package provides functionality that, when used together with the
-- <https://hackage.haskell.org/package/rapid rapid library>, can open a
-- persistent terminal that the application can access directly, such
-- that terminal applications can be tested with the main GHCi instance.

module Rapid.Term
    ( -- * Tutorial
      -- $tutorial

      -- ** Vty
      -- $vty

      -- * Terminal support for Rapid
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
      urxvtAt,

      -- * Helper functions
      stats
    )
    where

import Control.Concurrent
import Control.Exception
import Control.Monad.Codensity
import Control.Monad.IO.Class
import Data.IORef
import System.Clock
import System.IO
import System.Mem.Weak
import System.Posix.IO
import System.Posix.Terminal
import System.Posix.Types
import System.Process
import Text.Printf


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


-- | Write execution diagnostics for the given action to the given
-- terminal

stats :: MVar Term -> IO a -> IO ()
stats tRef c =
    terminal tRef $ \h -> do
        hPutStrLn h "\n--- App start"
        rt0 <- getTime Monotonic
        ct0 <- getTime ProcessCPUTime
        mx <- try c
        ct1 <- getTime ProcessCPUTime
        rt1 <- getTime Monotonic
        case mx of
          Left (SomeException ex) -> do
              hPutStrLn h "*** Unhandled exception:"
              hPutStr h . unlines . map ("      " ++) . lines . show $ ex
          Right _ -> hPutStrLn h "--- App stop"
        let dt t0 t1 = fromInteger (toNanoSecs (t1 - t0)) / 1e9 :: Double
        hPrintf h "Real time: %8.4f secs\n" (dt rt0 rt1)
        hPrintf h "CPU time:  %8.4f secs\n" (dt ct0 ct1)


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


{- $tutorial

This tutorial assumes that you are already familiar with the
<https://hackage.haskell.org/package/rapid rapid library>, and that you
use <http://software.schmorp.de/pkg/rxvt-unicode.html rxvt-unicode> (or
at least have it installed).

Say you are writing a terminal application that requires an actual
terminal that you would like to test during development.  For example
you are using ANSI control sequences, or perhaps you're even using a
text UI based on <https://hackage.haskell.org/package/vty Vty>.  Ideally
you could use the running GHCi instance, but if you're using an editor
like Emacs and haskell-interactive-mode, then that's not possible
/directly/, because it's not attached to a terminal.

This library provides a way to fire up a separate, potentially
persistent terminal as a subprocess and communicate with it through one
or more 'Handle's.

The first step to using this library is to abstract over the 'Handle's
you want to use:

> module Main (main) where
>
> import System.IO
>
> mainWith :: Handle -> Handle -> Handle -> IO ()
> mainWith hI hO hE = {- ... -}
>
> main :: IO ()
> main = mainWith stdin stdout stderr

In other words: you no longer use the built-in handles, but do all your
input and output in @mainWith@ by reading from and writing to the
handles explicitly passed to it.  Let's use an example program that
reads a line from the input handle and writes it to the output handle:

> import Control.Concurrent
> import System.IO
>
> mainWith :: Handle -> Handle -> Handle -> IO ()
> mainWith hI hO _ = do
>     hPutStr hO "Type something: "
>     hFlush hO
>     line <- hGetLine hI
>
>     hPutStrLn hO "Wait for it..."
>     threadDelay 2000000
>     hPutStrLn hO ("You typed: " ++ line)

Now in your @DevelMain@ module you need three things:

  * a terminal reference,

  * a terminal thread,

  * a thread that calls your application.

This amounts to the following @update@ action:

> module DevelMain (update) where
>
> import Main (mainWith)
> import Rapid
> import Rapid.Term
>
> update :: IO ()
> update =
>     rapid 0 $ \r -> do
>         -- Create the terminal reference
>         t <- createRef r "term-ref" newTermRef
>
>         -- Thread for the terminal
>         start r "term" (runTerm urxvt t)
>
>         -- Thread for your application
>         restart r "my-app" . terminal t $ \h ->
>             mainWith h h h

Now if you use @update@ an rxvt-unicode terminal will pop up and run
@mainWith@, which will prompt you to type something.  Once you type a
line into that terminal, @mainWith@ will finish.  When you @update@
again, it will start over in the same terminal.  If you actually want to
open a new terminal every invocation, just use @restart@ instead of
@start@ for the terminal thread.

You can have as many application threads using the terminal concurrently
as you want.  Also you can request multiple handles to the terminal
e.g. to have different buffering modes for each:

> restart r "my-app" . terminal t $ \hI ->
>     terminal t $ \hO ->
>         mainWith hI hO hO

If you would like to see a few diagnostics after each application run,
just wrap your terminal action by 'stats':

> restart r "test-app" . stats t . terminal t $ \h ->
>     mainWith h h h

This also makes it easier to see when the application is finished,
because otherwise there would be no indication.

Note: While we have abstracted over three handles above there is no
technical reason to do that.  If you don't actually use, say, stderr in
your application, there is no reason to abstract over it:

> mainWith :: Handle -> Handle -> IO ()
> mainWith hI hO = {- ... -}

-}


{- $vty

Running Vty applications requires some minor setup to work properly.
First of all instead of abstracting over input/output handles you should
abstract over the @Vty@ handle instead.  Let's write a very simple
example application:

> module Main (main) where
>
> import Graphics.Vty
>
> mainWith :: Vty -> IO ()
> mainWith vty = go ""
>     where
>     go inp = do
>         let pic = picForImage $
>                   string defAttr "Type some text:" <->
>                   string defAttr inp
>
>         update vty pic
>         ev <- nextEvent vty
>         case ev of
>           EvKey (KChar c) _ -> go (inp ++ [c])
>           EvKey KEsc _ -> pure ()
>           _ -> go inp
>
> main :: IO ()
> main = do
>     cfg <- standardIOConfig
>     bracket (mkVty cfg) shutdown mainWith

Now in your @DevelMain@ module you create the terminal reference and
thread as usual, but in your application thread you use 'termFd' to get
a file descriptor instead of a 'Handle', which is exactly what Vty
needs:

> module DevelMain (update) where
>
> import Control.Exception
> import qualified Graphics.Vty as Vty
> import Rapid
> import Rapid.Term
>
> update :: IO ()
> update =
>     rapid 0 $ \r -> do
>         t <- createRef r "term-ref" newTermRef
>         start r "term" (runTerm urxvt t)
>         restart r "test-app" . stats t . termFd t $ \fd -> do
>             cfg' <- Vty.standardIOConfig
>             let cfg = cfg' {
>                         Vty.inputFd = Just fd,
>                         Vty.outputFd = Just fd,
>                         Vty.termName = Just "rxvt-unicode-256color"
>                       }
>             bracket (Vty.mkVty cfg) Vty.shutdown mainWith

So the main differences are that you need to tell Vty explicitly which
handles it should use, and that you should probably also set the name of
the terminal explicitly (@termName@) so that Vty can find its terminfo
database.

Now you can use Rapid to develop your Vty applications!

-}
