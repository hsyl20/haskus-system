{-# LANGUAGE LambdaCase #-}

module Haskus.Apps.System.Build.Linux
   ( linuxBuild
   )
where

import qualified Data.Text as Text
import Control.Monad

import Haskus.Apps.System.Build.Config
import Haskus.Apps.System.Build.Utils

linuxBuild :: LinuxConfig -> FilePath -> IO ()
linuxBuild config path = do
   let shell' = shellInErr path

   -- make default configuration for the arch
   shell' "make x86_64_defconfig"
      $ fail "Unable to build Linux default configuration"

   -- enable/disable/module options
   let opts = linuxOptions config
   forM_ (enableOptions opts) $ \opt ->
      shell' ("./scripts/config -e "++ Text.unpack opt)
         $ fail $ "Unable to enable Linux option: " ++ Text.unpack opt
   forM_ (disableOptions opts) $ \opt ->
      shell' ("./scripts/config -d "++ Text.unpack opt)
         $ fail $ "Unable to disable Linux option: " ++ Text.unpack opt
   forM_ (moduleOptions opts) $ \opt ->
      shell' ("./scripts/config -m "++ Text.unpack opt)
         $ fail $ "Unable to modularize Linux option: " ++ Text.unpack opt

   -- fixup config (interactive)
   shell' "make oldconfig"
      $ fail "Unable to adjust Linux configuration"

   -- build
   shell' ("make " ++ Text.unpack (linuxMakeArgs config))
      $ fail "Unable to build Linux"
