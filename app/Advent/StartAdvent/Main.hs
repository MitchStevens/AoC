module StartAdvent.Main where

import DownloadInput

import Control.Monad
import Options.Applicative
import Shelly
import Text.Printf
import Data.Text (Text)
import qualified Data.Text as T
import Types

data Options = Options
  { adventDate :: AdventDate
  , language :: Language
  }

optionsP :: Parser Options
optionsP = Options <$> adventDateP <*> languageP

-- create folder
-- create Main.hs
-- download input
startAdvent :: Options -> Sh ()
startAdvent (Options date@(AdventDate year day) language) = do
  whenM (test_f file) $ do
     echo "Advent has already been started"
     exit 0
  
  mkdir_p fileDir
  touchfile file
  writefile file mainContent
  downloadInput date 
  exit 0

  where
    file = fileDir <> "Main.hs"

    fileDir :: FilePath
    fileDir = case language of
      Haskell -> printf "app/%d/Day%d/" year day

    mainContent :: Text
    mainContent = case language of
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


