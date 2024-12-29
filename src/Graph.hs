module Graph where
--https://gist.github.com/abhin4v/8172534

import Data.Function (on)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map, (!))
import qualified Data.Map as M

type Path a = [a]

--dijkstra
--  :: (Eq a)
--  => a
--  -> (a -> [(a, Int)])
--  -> Map a (a, Int)
--dijkstra goal next = dijkstraRec (PQ.singleton 0 goal) (S.singleton goal)
--  where
--    dijkstraRec :: PQ.MinPQueue Int a -> Set a -> Map a (a, Int) -> Map a (a, Int)
--    dijkstraRec open seen m =
--      if PQ.null open
--        then M.empty
--        else dijkstraRec open' seen' m'
--
--      where
--        ((u, distU), open') = PQ.deleteFindMin open
--
--        -- Add the node to the closed set
--        seen' =  S.insert node seen
--
--        updates :: Map a (a, Int)
--        updates = M.fromListWith (min `on` snd) $ flip map (next u) $ \(v, distUV) ->
--          let alt = snd (m ! u) + distance
--              distV = M.lookup v m
--          in 
--            --if ((alt <) <$> M.lookup v m) == Just True
--            --  then M.singleton v (u, alt)
--            --  else M.empty
--        
--        m' = M.unionWith (min `on` snd) m updates
--

