{-# LANGUAGE LambdaCase #-}
module ViperVM.Arch.Linux.Error
   ( Sys
   , runSys
   , runSys'
   , Log (..)
   , LogType (..)
   , sysLog
   , sysLogSequence
   , sysAssert
   , sysCallAssert
   , sysCallAssert'
   , sysCallAssertQuiet
   , sysCallWarn
   , sysLogPrint
   )
where

import Prelude hiding (log)
import Data.Dequeue as DQ
import Data.Text.Lazy as Text
import Data.Text.Lazy.IO as Text
import Data.String (fromString)
import qualified Data.Text.Format as Text
import Control.Monad.State
import Data.Foldable (traverse_)
import Text.Printf

import ViperVM.Arch.Linux.ErrorCode

------------------------------------------------
-- Sys monad
------------------------------------------------

type Sys a = StateT SysState IO a

data SysState = SysState
   { sysLogCurrent :: Log         -- ^ Current log
   , sysLogStack   :: [Log]       -- ^ Stack of logs
   }

-- | Run
runSys :: Sys a -> IO a
runSys act = evalStateT act initState
   where
      initState = SysState
         { sysLogCurrent = Log (Text.pack "Log root") LogSequence DQ.empty
         , sysLogStack   = []
         }

-- | Run and return nothing
runSys' :: Sys a -> IO ()
runSys' = void . runSys

-- | Called on system error
sysOnError :: Sys a
sysOnError = do
   -- close the log
   sysLogClose

   -- print the log
   sysLogPrint

   -- fail
   error "System failed"

------------------------------------------------
-- Logging
------------------------------------------------

-- | Hierarchical log
data Log = Log
   { logValue    :: Text                 -- ^ Log value
   , logType     :: LogType              -- ^ Log type
   , logChildren :: BankersDequeue Log   -- ^ Log children
   } deriving (Show)

data LogType
   = LogDebug
   | LogInfo
   | LogWarning
   | LogError
   | LogSequence
   deriving (Show,Eq)

-- | Add a new entry to the log
sysLog :: LogType -> String -> Sys ()
sysLog typ text = sysLogAppend (Log (Text.pack text) typ DQ.empty)

-- | Add a new sequence of actions to the log
sysLogSequence :: String -> Sys a -> Sys a
sysLogSequence text act = do
   sysLogBegin (Text.pack text)
   r <- act
   sysLogEnd
   return r

-- | Append a log entry to the current log sequence
sysLogAppend :: Log -> Sys ()
sysLogAppend e = do
   s <- get
   let
      log = sysLogCurrent s
      log' = log { logChildren = DQ.pushBack (logChildren log) e }
   put $ s { sysLogCurrent = log' }

-- | Start a new log sequence
sysLogBegin :: Text -> Sys ()
sysLogBegin text = do
   s <- get
   let
      current = sysLogCurrent s
      stack   = sysLogStack s
   
   put $ s
      { sysLogCurrent = Log text LogSequence DQ.empty
      , sysLogStack   = current : stack
      }

-- | End a log sequence
sysLogEnd :: Sys ()
sysLogEnd = do
   s <- get
   let
      current = sysLogCurrent s
      (x:xs)  = sysLogStack s
   -- attach the current log to the parent one
   put $ s
      { sysLogCurrent = x
      , sysLogStack   = xs
      }
   sysLogAppend current

-- | Close the log
sysLogClose :: Sys ()
sysLogClose = gets sysLogStack >>= \case
   [] -> return ()
   _  -> sysLogEnd >> sysLogClose

-- | Print the log on the standard output
sysLogPrint :: Sys ()
sysLogPrint = do
      -- print the log
      log <- gets sysLogCurrent
      lift $ printLog 0 log
   where
      formatLog i l = Text.format (fromString "{}--{}- {}{}")
         ( Text.replicate i (Text.pack "  |")
         , if DQ.null (logChildren l) then "-" else "+"
         , Text.pack $ case logType l of
            LogSequence -> ""
            LogWarning  -> "Warning: "
            LogError    -> "Error: "
            LogDebug    -> "Debug: "
            LogInfo     -> ""
         , logValue l
         )
      printLog i l = do
         Text.putStrLn (formatLog i l)
         traverse_ (printLog (i+1)) (logChildren l)

sysAssert :: String -> Bool -> Sys ()
sysAssert text b = case b of
   True -> do
      let msg = printf "%s (success)" text
      sysLog LogInfo msg
   False -> do
      let msg = printf "%s (assertion failed)" text
      sysLog LogError msg
      sysOnError

------------------------------------------------
-- System calls
------------------------------------------------

-- | Assert that the given action doesn't fail
sysCallAssert :: String -> SysRet a -> Sys a
sysCallAssert text act = do
   r <- lift act
   sysCallAssert' text r

-- | Assert that the given action doesn't fail
sysCallAssert' :: String -> Either ErrorCode a -> Sys a
sysCallAssert' text r = do
   case r of
      Left err -> do
         let msg = printf "%s (failed with %s)" text (show err)
         sysLog LogError msg
         sysOnError
      Right v  -> do
         let msg = printf "%s (success)" text
         sysLog LogInfo msg
         return v

-- | Assert that the given action doesn't fail, log only on error
sysCallAssertQuiet :: String -> SysRet a -> Sys a
sysCallAssertQuiet text act = do
   r <- lift act
   case r of
      Left err -> do
         let msg = printf "%s (failed with %s)" text (show err)
         sysLog LogError msg
         sysOnError
      Right v  -> return v

-- | Log a warning if the given action fails
sysCallWarn :: String -> SysRet a -> Sys (Either ErrorCode a)
sysCallWarn text act = do
   r <- lift act
   case r of
      Left err -> do
         let msg = printf "%s (failed with %s)" text (show err)
         sysLog LogWarning msg
      Right _  -> do
         let msg = printf "%s (success)" text
         sysLog LogInfo msg
   return r
