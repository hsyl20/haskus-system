{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import GHCWeb.Build
import GHCWeb.CmdLine (Options(..), getOptions)

import ViperVM.Utils.Embed

import Control.Monad
import Happstack.Server
import Lucid
import qualified ViperVM.Format.Text as Text
import ViperVM.Format.Text (textFormat,Only(..))
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LBS
import Control.Monad.IO.Class

main :: IO ()
main = do
   opts <- getOptions
   server (nullConf { port = optport opts} )

server :: Conf -> IO ()
server conf = do
   Text.putStrLn (textFormat "Starting Web server at localhost: {}" (Only $ port conf))

   let ok' = ok . toResponse . renderBS . appTemplate

   simpleHTTP conf $ msum
      [ dir "css" $ dir "style.css" $ ok css

      , nullDir >> ok' welcomePage

      , dir "build" $ nullDir >> do
         res <- liftIO build
         ok' (buildPage res)

      ]

buildPage :: String -> Html ()
buildPage s = do
   h1_ "Build"
   p_ (toHtml s)

welcomePage :: Html ()
welcomePage = do
   h1_ "Welcome"

appTemplate :: Html () -> Html ()
appTemplate doc = do
   head_ $ do
      title_ "GHC Web Interface"
      meta_ [ httpEquiv_ "Content-Type"
            , content_ "text/html;charset=utf-8"
            ]
      link_ [ rel_  "stylesheet"
            , type_ "text/css"
            , href_ "/css/style.css"
            ]
   doc

css :: Response
css = toResponseBS
   (C.pack "text/css")
   (LBS.fromStrict $(embedFile "src/apps/Elf/style.css"))

