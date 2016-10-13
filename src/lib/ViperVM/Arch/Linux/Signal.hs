{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-- | Linux signals
module ViperVM.Arch.Linux.Signal
   ( SignalSet(..)
   , ChangeSignals(..)
   , sysPause
   , sysAlarm
   , sysSendSignal
   , sysSendSignalGroup
   , sysSendSignalAll
   , sysCheckProcess
   , sysChangeSignalMask
   )
where

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Syscalls
import ViperVM.Arch.Linux.Process
import ViperVM.Format.Binary.Vector (Vector)
import ViperVM.Format.Binary.Word
import ViperVM.Format.Binary.Storable
import ViperVM.Format.Binary.Layout.Struct
import ViperVM.Format.Binary.Ptr (Ptr,nullPtr)
import ViperVM.Utils.Flow
import ViperVM.Utils.Types.Generics

-- | Signal set
newtype SignalSet = SignalSet
   { signalSet :: Vector 16 Word64
   } deriving (Generic)

-- | Pause
sysPause :: SysRet ()
sysPause = onSuccess syscall_pause (const ())

-- | Alarm
sysAlarm :: Word-> SysRet Word
sysAlarm seconds =
   onSuccess (syscall_alarm seconds) fromIntegral

-- | Kill syscall
sysSendSignal :: ProcessID -> Int -> SysRet ()
sysSendSignal (ProcessID pid) sig =
   onSuccess (syscall_kill (fromIntegral pid) sig) (const ())

-- | Send a signal to every process in the process group of the calling process
sysSendSignalGroup :: Int -> SysRet ()
sysSendSignalGroup sig =
   onSuccess (syscall_kill 0 sig) (const ())

-- | Send a signal to every process for which the calling process has permission to send signals, except for process 1 (init)
sysSendSignalAll :: Int -> SysRet ()
sysSendSignalAll sig =
   onSuccess (syscall_kill (-1) sig) (const ())

-- | Check if a given process or process group exists
--
-- Send signal "0" the given process
sysCheckProcess :: ProcessID -> SysRet Bool
sysCheckProcess pid = sysSendSignal pid 0
   >.-.> const True
   >%~$> \case
      ESRCH -> flowRet0 False
      e     -> flowRet1 e

-- | Signal actions
data ChangeSignals
   = BlockSignals    -- ^ Block signals in the set
   | UnblockSignals  -- ^ Unblock signals in the set
   | SetSignals      -- ^ Set blocked signals to the set
   deriving (Show,Eq,Enum)

-- | Change signal mask
sysChangeSignalMask :: ChangeSignals -> Maybe SignalSet -> SysRet SignalSet
sysChangeSignalMask act set =
   withMaybeOrNull set $ \pset ->
      alloca $ \ret ->
         onSuccessIO (syscall_sigprocmask (fromEnum act) pset ret)
            (const $ peekStruct ret)
