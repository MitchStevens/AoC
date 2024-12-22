module DownloadInput where 

import Types
import Shelly
import Text.Printf
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad

downloadInput :: AdventDate -> Sh ()
downloadInput date@(AdventDate year day) = do
  whenM (test_f file) do
    echo (T.pack (printf "Input for %s has already been downloaded" (show date)))
    exit 0


    get_env "AOC_SESSION_COOKIE" >>= \case
      Nothing -> errorExit "Environment variable $AOC_SESSION_COOKIE was not set, unable to download input"
      Just cookie -> do
        input <- run "curl"
          [ T.pack (printf "https://adventofcode.com/%d/day/%d/input" year day)
          , "--cookie"
          , T.pack (printf "session=%s" cookie)
          , "-v"
          ]
        mkdir_p dir
        touchfile file
        writefile file input

  where
    dir = printf "inputs/%d" year
    file = dir <> printf "/day%d.txt" day