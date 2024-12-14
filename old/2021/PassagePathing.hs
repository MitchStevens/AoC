module PassagePathing where
import Data.Set (Set)
import Data.Map (Map, (!))
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe (isNothing, fromJust)
import Control.Lens

data Cave
    = Start
    | Big String
    | Small String
    | End
    deriving (Eq, Ord, Show)

type CaveSystem = Map Cave [Cave] 
type Path = [Cave]

makeCaveSystem :: [(Cave, Cave)] -> CaveSystem
makeCaveSystem = undefined -- foldl (flip addConnection) M.empty
    where
        addConnection (Start, b) = 
            M.insertWith (<>) Start (S.singleton b)
        addConnection (a, Start) = 
            M.insertWith (<>) Start (S.singleton a)
        addConnection (End, b) = 
            M.insertWith (<>) End (S.singleton b)
        addConnection (a, End) = 
            M.insertWith (<>) End (S.singleton a)
        addConnection (a, b) = 
            M.insertWith (<>) a (S.singleton b) . M.insertWith (<>) b (S.singleton a)

paths :: CaveSystem -> Cave -> Cave -> Set Path
paths caves from to = 
    if from == to
        then S.singleton [to]
        else foldMap (paths caves) (caves ! from) to
            & S.map (appendPath from)
            & S.filter isNothing
            & S.map fromJust

appendPath :: Cave -> Path -> Maybe Path
appendPath c path = case c of
    Start -> if Start `elem` path then Nothing else Just (c : path)
    End -> if null path then Just [End] else Nothing
    Big _ -> Just (c : path)
    Small _ -> if c `elem` path then Nothing else Just (c : path)
