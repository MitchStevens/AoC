module NoSpaceLeftOnDevice where

import Advent
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad (foldM, forM_)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.Functor.Classes (Show1 (liftShowsPrec))
import Data.Functor.Foldable
import Control.Comonad.Cofree
import Control.Comonad.Trans.Cofree (headF, tailF, CofreeF)
import Data.Fix (Fix(Fix))
import Control.Comonad (Comonad(extract, extend))


data FileSystemF a = Folder (Map String a) | File Int
    deriving (Show, Functor, Foldable)

instance Show1 FileSystemF where
    liftShowsPrec f g n = \case
        Folder children -> mappend (replicate (2*n) ' ') . mappend "folder \n" . liftShowsPrec f (\l -> mappend "  - " . g l) n (M.elems children) . mappend "  \n"
        File fileSize -> mappend (replicate (2*n) ' ' <> "file " <> show fileSize) . mappend "\n"

type FileSystem = Fix FileSystemF

data FileSystemZ = NodeZ
    { name :: String
    , children :: Map String FileSystem
    , parent :: Maybe FileSystemZ
    }

data Command
    = Ls
    | Cd String
    | FileInfo String Int
    | Dir String
    deriving (Eq, Show)

file :: Int -> FileSystem
file n = Fix (File n)

folder :: Map String FileSystem -> FileSystem
folder children = Fix (Folder children)

testInput = [ "$ cd /" , "$ ls" , "dir a" , "14848514 b.txt" , "8504156 c.dat" , "dir d" , "$ cd a" , "$ ls" , "dir e" , "29116 f" , "2557 g" , "62596 h.lst" , "$ cd e" , "$ ls" , "584 i" , "$ cd .." , "$ cd .." , "$ cd d" , "$ ls" , "4060174 j" , "8033020 d.log" , "5626152 d.ext" , "7214296 k" ]

parseCommand :: String -> Command
parseCommand str = case str of
    '$':' ':'c':'d':' ':xs -> Cd xs
    "$ ls" -> Ls
    'd':'i':'r':' ':xs -> Dir xs
    _ -> let [size, filename] = words str in FileInfo filename (read size)

mkdir :: String -> FileSystemZ -> FileSystemZ
mkdir dirName (NodeZ name children parent) = NodeZ name (M.alter (Just . fromMaybe (folder M.empty)) dirName children) parent

touch :: String -> Int -> FileSystemZ -> FileSystemZ
touch fileName fileSize (NodeZ name children parent) = NodeZ name (M.insert fileName (file fileSize) children) parent

cd :: String -> FileSystemZ -> Maybe FileSystemZ
cd ".." (NodeZ name children Nothing) = Nothing
cd ".." (NodeZ name children (Just (NodeZ parentName siblings grandParent))) = Just (NodeZ parentName (M.insert name (folder children) siblings) grandParent)
cd dirName (NodeZ name children parent) = do
    Fix (Folder grandChildren) <- M.lookup dirName children
    Just (NodeZ dirName grandChildren (Just (NodeZ name (M.delete dirName children) parent)))

reifyFileSystem :: FileSystemZ -> FileSystem
reifyFileSystem (NodeZ name children Nothing) = folder children
reifyFileSystem (NodeZ name children (Just (NodeZ parentName siblings grandParent))) = reifyFileSystem (NodeZ parentName (M.insert name (folder children) siblings) grandParent)

buildFileSystem :: FileSystemZ -> Command -> Maybe FileSystemZ
buildFileSystem fsz = \case
    Ls -> Just fsz
    Cd s -> if s == ".." then cd s fsz else cd s (mkdir s fsz)
    FileInfo fileName fileSize -> Just (touch fileName fileSize fsz)
    Dir fileName -> Just (mkdir fileName fsz)


asCofree :: Functor f => Fix f -> Cofree f ()
asCofree (Fix f) = () :< (asCofree <$> f)

fileSystemSize :: FileSystemF Int -> Int
fileSystemSize = \case
    Folder children -> sum children
    File fileSize -> fileSize

withFileSize :: Cofree FileSystemF () -> Cofree FileSystemF Int
withFileSize = \case
    () :< Folder children -> let children' = fmap withFileSize children in (sum . fmap sum) children' :< Folder children' -- Folder children'
    () :< File fileSize -> fileSize :< File fileSize

isSmallDir :: Cofree FileSystemF Int -> Int
isSmallDir dir = case dir of
    (n :< Folder _) -> if n <= 100000 then n else 0
    _ -> 0

day7 :: IO ()
day7 = do
    let input = testInput
    --input <- readInput 2022 7
    let commands = map parseCommand input

    forM_ (reifyFileSystem <$> foldM buildFileSystem (NodeZ "/" M.empty Nothing) commands) $ \fs -> do
        -- cata :: (FileSystemF Int -> Cofree FileSystemF Int) -> Fix FileSystemF -> Cofree Filesystem Int
        let (fsWithSizes :: Cofree FileSystemF Int) = withFileSize (asCofree fs)
        print fsWithSizes
        print $ sum $ extend isSmallDir fsWithSizes