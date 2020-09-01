{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logInfoN, runStderrLoggingT)
import qualified Data.Text as Text
import Data.Yaml (decodeFileThrow)
import Git (gitRev)
import Network.Wai.Handler.Warp (HostPreference, defaultSettings, setHost, setPort)
import Options.Applicative
  ( CommandFields,
    Mod,
    Parser,
    argument,
    auto,
    command,
    customExecParser,
    disambiguate,
    fullDesc,
    help,
    helper,
    idm,
    info,
    infoOption,
    long,
    metavar,
    option,
    prefs,
    progDesc,
    short,
    showDefault,
    showHelpOnEmpty,
    showHelpOnError,
    str,
    strOption,
    subparser,
    value,
  )
import qualified PSGenerator

-- | You might wonder why we don't stick everything in `Config`. The
-- answer is that pushing certain flags to the command line makes
-- automated deployment easier.
--
-- You might also wonder why we don't stick everything on the command
-- line. The answer is for flags that rarely change, putting them in a
-- config file makes development easier.
data Command = PSGenerator {_outputDir :: !FilePath}
  deriving (Show, Eq)

versionOption :: Parser (a -> a)
versionOption =
  infoOption
    (Text.unpack gitRev)
    (short 'v' <> long "version" <> help "Show the version")

commandLineParser :: Parser (FilePath, Command)
commandLineParser = (,) <$> configFileParser <*> commandParser

configFileParser :: Parser FilePath
configFileParser =
  option
    str
    ( long "config" <> metavar "CONFIG_FILE" <> help "Config file location."
        <> value "playground.yaml"
    )

commandParser :: Parser Command
commandParser = subparser psGeneratorCommandParser

psGeneratorCommandParser :: Mod CommandFields Command
psGeneratorCommandParser =
  command "psgenerator" $
    flip info (fullDesc <> progDesc "Generate the frontend's PureScript files.") $ do
      _outputDir <-
        argument
          str
          ( metavar "OUTPUT_DIR"
              <> help "Output directory to write PureScript files to."
          )
      pure PSGenerator {..}

runCommand :: (MonadIO m, MonadLogger m) => FilePath -> Command -> m ()
runCommand _ PSGenerator {..} = liftIO $ PSGenerator.generate _outputDir

main :: IO ()
main = do
  options <-
    customExecParser
      (prefs $ disambiguate <> showHelpOnEmpty <> showHelpOnError)
      (info (helper <*> versionOption <*> commandLineParser) idm)
  runStderrLoggingT $ do
    logInfoN $ "Running: " <> Text.pack (show options)
    uncurry runCommand options
