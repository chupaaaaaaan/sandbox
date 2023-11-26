{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Import
import Options.Applicative.Simple
import qualified Paths_jep_crawler
import RIO.Process
import Run

main :: IO ()
main = do
    (options, ()) <-
        simpleOptions
            $(simpleVersion Paths_jep_crawler.version)
            "Header for command line arguments"
            "Program description, also for command line arguments"
            ( Options
                <$> switch
                    ( long "verbose"
                        <> short 'v'
                        <> help "Verbose output?"
                    )
                <*> strOption
                    ( long "output-file"
                        <> short 'o'
                        <> metavar "<FILE>"
                        <> value "output.csv"
                        <> help "Output file name"
                    )
                <*> option auto
                    ( long "target-jdk"
                        <> short 'j'
                        <> metavar "<JDK Version>"
                        <> value Jdk9
                        <> help "Target JDK verion"
                    )
                <*> switch
                    ( long "do-translate"
                        <> short 't'
                        <> help "Translate "
                    )
            )
            empty
    lo <- logOptionsHandle stderr options.optionsVerbose
    pc <- mkDefaultProcessContext
    withLogFunc lo $ \lf ->
        let app =
                App
                    { appLogFunc = lf
                    , appProcessContext = pc
                    , appOptions = options
                    }
         in runRIO app run
