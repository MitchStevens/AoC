CycleData = CycleData
  { offset :: Int
  , period :: Int
  }

analyseCycles :: (a -> b -> a) -> [a] -> Maybe CycleData
analyseCycles f list

bigFoldl' 