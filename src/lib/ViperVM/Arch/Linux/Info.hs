{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- | System info (uname)
module ViperVM.Arch.Linux.Info
   ( SystemInfo(..)
   , systemInfo
   )
where

import ViperVM.Arch.Linux.ErrorCode
import ViperVM.Arch.Linux.Syscalls
import ViperVM.Format.Binary.Storable
import ViperVM.Format.Binary.Layout.Struct
import ViperVM.Format.String
import ViperVM.Utils.Types.Generics

-- | struct utsname
data SystemInfo = SystemInfo
   { systemName     :: CStringBuffer 65
   , systemNodeName :: CStringBuffer 65
   , systemRelease  :: CStringBuffer 65
   , systemVersion  :: CStringBuffer 65
   , systemMachine  :: CStringBuffer 65
   } deriving (Show,Generic)

-- | "uname" syscall
systemInfo :: SysRet SystemInfo
systemInfo = alloca @(Struct SystemInfo) $ \ptr ->
   onSuccessIO (syscall_uname ptr) (const (peek ptr))
