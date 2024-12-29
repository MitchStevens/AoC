module RunAdvent.Main where

import Types
import DownloadInput
import Shelly
import Options.Applicative
import Text.Printf
import Data.Text as T

data Options = Options
  { adventDate :: AdventDate
  , language :: Language
  , watch :: Bool
  }

optionsP :: Parser Options
optionsP = Options <$> adventDateP <*> languageP <*> watchP

runOnModification :: Text -> Text -> Sh ()
runOnModification sourceFiles command = run_
  (printf "echo '%s' | entr -s '%s'" sourceFiles command) []

runAdvent :: Options -> Sh ()
runAdvent (Options date@(AdventDate year day) language watch) = do
  downloadInput date
  let adventProgram = case language of {
    Haskell -> T.pack (printf "stack run advent-%d-%d --color always" year day)
  }
  if watch
    then do
      (sourceFiles :: Text)  <- case language of
        Haskell -> run "find" ["src app test", "-name", "*.hs"]
      runOnModification sourceFiles adventProgram
    else cmd (T.unpack adventProgram)

main :: IO ()
main = do
  opts <- execParser (info optionsP fullDesc) 
  shelly (runAdvent opts)