module Main (main) where

import Control.Applicative ((<*), (<**>), (<*>))
import Options.Applicative (
  Parser,
  ParserInfo,
  auto,
  command,
  customExecParser,
  fullDesc,
  help,
  helper,
  info,
  long,
  metavar,
  option,
  prefs,
  progDesc,
  showDefault,
  showHelpOnEmpty,
  showHelpOnError,
  strOption,
  subparser,
  value,
 )
import Scripts.Compile (CompileMode (COMPILE_DEBUG, COMPILE_PROD), CompileOpts (CompileOpts), compile)
import Prelude (IO, ($), (<$>), (<>))

newtype Command = Compile CompileOpts

compileOpts :: Parser CompileOpts
compileOpts =
  CompileOpts
    <$> option
      auto
      ( long "mode"
          <> metavar "COMPILE_MODE"
          <> help "Mode of compilation COMPILE_DEBUG|COMPILE_PROD"
          <> value COMPILE_PROD
          <> value COMPILE_DEBUG
          <> showDefault
      )
    <*> strOption
      ( long "file"
          <> metavar "COMPILE_FILE"
          <> help "A JSON file to store the compiled scripts"
          <> value "triphut-dao-scripts.json"
          <> showDefault
      )

options :: Parser Command
options =
  subparser $
    command
      "compile"
      (info (Compile <$> compileOpts <* helper) (progDesc "Compile scripts and write them to a file"))

parserInfo :: ParserInfo Command
parserInfo = info (options <**> helper) (fullDesc <> progDesc "Create Scripts CLI")

main :: IO ()
main = do
  cmd <- customExecParser (prefs (showHelpOnEmpty <> showHelpOnError)) parserInfo
  case cmd of
    Compile opts -> compile opts
