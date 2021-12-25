module Common.Algorithm where

import Data.PQueue.Prio.Min (null, insert, singleton, deleteFindMin)
import Data.Set (member, insert, empty, notMember)
import Data.Map (singleton, insertWith)

search :: Ord a => (a -> Bool) -> (a -> [(a, Integer)]) -> a -> Maybe (a, Integer)
search f g i = go Data.Set.empty (Data.PQueue.Prio.Min.singleton 0 i) (Data.Map.singleton i 0)
    where
        go a x d
            | Data.PQueue.Prio.Min.null x = Nothing
            | f u = Just (u, w)
            | Data.Set.member u a = go na nx' nd
            | otherwise = go na nx nd
            where
                ((w, u), nx') = deleteFindMin x
                na = Data.Set.insert u a
                nb = [(p, w + nw) | (p, nw) <- g u, Data.Set.notMember p na]
                nx = foldl (\m (n, wt) -> Data.PQueue.Prio.Min.insert wt n m) nx' nb
                nd = foldl (\m (n, wt) -> Data.Map.insertWith min n wt m) d nb
