module StartAdvent.Main where

import DownloadInput

import Control.Monad
import Options.Applicative
import Shelly
import Text.Printf
import Data.Text (Text)
import qualified Data.Text as T
import Types

startAdvent :: Options -> Sh ()
startAdvent (Options date@(AdventDate year day) language) = do
  whenM (test_f file) $
     echo_err ("Advent has already been started")
  
  mkdir_p fileDir
  touchfile file
  writefile file contents

  -- generatePackacke year day
  downloadInput date 

  where
    file = fileDir <> "Main.hs"

    fileDir :: FilePath
    fileDir = case language of
      Haskell -> printf "app/%d/Day%d/" year day

    contents :: Text
    contents = case language of
      Haskell -> T.unlines
        [ "import Advent"
        , ""
        , "main :: IO ()"
        , "main = do"
        , T.pack (printf "  input <- readInput %d %d" year day)
        , "  print \"hello world\""
        ]

main :: IO ()
main = do
  opts <- execParser (info optionsP fullDesc)
  shelly (startAdvent opts)


