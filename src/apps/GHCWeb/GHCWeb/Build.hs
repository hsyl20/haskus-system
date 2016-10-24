module GHCWeb.Build where

import GHC.Paths (libdir)
import GHC
import Outputable
import DynFlags
import qualified Parser
import Lexer
import SrcLoc
import FastString
import StringBuffer
import ErrUtils
import Data.Time.Clock (getCurrentTime, UTCTime)

makeTarget :: StringBuffer -> UTCTime -> Target
makeTarget buf tim = Target
   { targetId           = TargetModule (mkModuleName "Test")
   , targetAllowObjCode = False
   , targetContents     = Just (buf,tim)
   }

makeTargetString :: String -> IO Target
makeTargetString s = do
   t <- getCurrentTime
   return (makeTarget (stringToStringBuffer s) t)
 
build :: IO String
build = do
   let test = 
            "module Test where\n\
            \  f :: Int -> Int\n\
            \  f x = x * 2"
   tgt <- makeTargetString test
   res <- example tgt
   runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      let p = parser test dflags "<interactive>"
      case p of
         Left errs       -> return $ showSDoc dflags $ vcat (pprErrMsgBagWithLoc errs)
         Right (warns,a) -> return $ showSDoc dflags $ ppr a
 
example :: Target -> IO (ParsedSource, String, TypecheckedSource)
example target = 
--    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
      runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        let dflags' = dflags
               { ghcMode = OneShot
               }
        setSessionDynFlags dflags'
        setTargets [target]
        load LoadAllTargets
        modSum <- getModSummary $ mkModuleName "Test"
        p <- parseModule modSum
        t <- typecheckModule p
        d <- desugarModule t
        -- l <- loadModule d
        -- n <- getNamesInScope
        -- c <- return $ coreModule d
 
        -- g <- getModuleGraph
        -- mapM showModule g     
        return $ (parsedSource d,"/n-----/n",  typecheckedSource d)
