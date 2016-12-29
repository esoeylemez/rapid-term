-- |
-- Copyright:  (c) 2016 Ertugrul Söylemez
-- License:    BSD3
-- Maintainer: Ertugrul Söylemez <esz@posteo.de>

{-# LANGUAGE RankNTypes #-}

module Rapid.Term
    ( -- * Terminal support for Rapid
      Term,
      startTerm,
      stopTerm,
      terminal,
      termFd,

      -- * Supported terminal emulators
      -- ** rxvt-unicode
      urxvt,
      urxvtc,
      urxvtAt
    )
    where

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


-- | Handle to a terminal.

data Term =
    Term {
      _processRef   :: IORef ProcessHandle,
      _processWeak  :: Weak (IORef ProcessHandle),
      _ttySlaveRef  :: IORef Fd,
      _ttySlaveWeak :: Weak (IORef Fd)
    }


cBracket :: IO a -> (a -> IO b) -> Codensity IO a
cBracket o c = Codensity (bracket o c)


cOnException :: IO a -> Codensity IO ()
cOnException c = Codensity (\k -> k () `onException` c)


startTerm :: (Fd -> IO ProcessHandle) -> IO Term
startTerm start =
    mask $ \unmask -> lowerCodensity $ do
        (master, slave) <- liftIO (unmask openPseudoTerminal)

        masterRef <- liftIO (newIORef master)
        masterWeak <- liftIO (mkWeakIORef masterRef (closeFd master))
        cOnException (finalize masterWeak)

        slaveRef <- liftIO (newIORef slave)
        slaveWeak <- liftIO (mkWeakIORef slaveRef (closeFd slave))
        cOnException (finalize slaveWeak)
        liftIO (unmask (setFdOption master CloseOnExec False))

        ph <- liftIO (unmask (start master))
        phRef <- liftIO (newIORef ph)
        let term = terminateProcess ph >> () <$ waitForProcess ph
        phWeak <- liftIO (mkWeakIORef phRef term)
        cOnException (finalize phWeak)
        liftIO (finalize masterWeak)

        pure (Term {
                _processRef   = phRef,
                _processWeak  = phWeak,
                _ttySlaveRef  = slaveRef,
                _ttySlaveWeak = slaveWeak
              })


stopTerm :: Term -> IO ()
stopTerm t = do
    finalize (_ttySlaveWeak t)
    finalize (_processWeak t)


termFd :: Term -> (Fd -> IO r) -> IO r
termFd t =
    bracket (readIORef (_ttySlaveRef t) >>= dup)
            closeFd


terminal :: Term -> (Handle -> Handle -> IO r) -> IO r
terminal t k =
    mask $ \unmask -> lowerCodensity $ do
        let mkTtyHandle =
                liftIO $
                    unmask (readIORef (_ttySlaveRef t) >>= dup) >>=
                    fdToHandle

        hI <- cBracket mkTtyHandle hClose
        hO <- cBracket mkTtyHandle hClose

        let conf h = do
                hSetBinaryMode h False
                hSetBuffering h LineBuffering
                hSetEcho h True
                hSetEncoding h localeEncoding
                hSetNewlineMode h nativeNewlineMode

        liftIO . unmask $ do
            traverse conf [hI, hO]
            k hI hO


urxvt :: Fd -> IO ProcessHandle
urxvt = urxvtAt "urxvt"


urxvtc :: Fd -> IO ProcessHandle
urxvtc = urxvtAt "urxvtc"


urxvtAt :: FilePath -> Fd -> IO ProcessHandle
urxvtAt p fd = spawnProcess p ["-pty-fd", show fd]
