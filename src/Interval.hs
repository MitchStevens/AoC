module Interval where

data Range a = Range { lower :: a, upper :: a }

data Region a
    = Empty
    | Interval a a
    | Disjoint (Region a) (Region a)

range :: Region a -> Maybe (Range a)
range Empty = Nothing
range (Interval a b) = Just (Range a b)
range (Disjoint r1 r2) = do
    Range a _ <- range r1
    Range _ b <- range r2
    pure (Range a b)
