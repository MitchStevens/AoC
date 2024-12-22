module RunAdvent.Main where

import Types
import Shelly
import Options.Applicative

runAdvent :: Options -> Sh ()
runAdvent (Options date language) = do
  downloadInput date
  case language of
    Haskell -> do
      generatePackage
      echo "hello world"

main :: IO ()
main = do
  opts <- execParser (info optionsP fullDesc) 
  shelly (runAdvent opts)