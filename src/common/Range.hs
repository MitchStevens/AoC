module Range where
import Data.Ix (Ix(inRange))

data Range a
  = Range a a

instance Show a => Show (Range a) where
  show (Range a b) = show a <> "-" <> show b

instance Ord a => Semigroup (Range a) where
  Range a1 b1 <> Range a2 b2 = Range (min a1 a2) (max b1 b2)

toRange :: Ord a => a -> a -> Range a
toRange a b = if a < b then Range a b else Range b a

union :: Range a -> Range a -> Range a 
union r1 r2 = undefined

intersect :: Range a -> Range a -> Range a 
intersect r1 r2 = undefined

isWithin :: ( Ix a) =>Range a -> Range a -> Bool
Range a1 b1 `isWithin` Range a2 b2 = inRange (a2, b2) a1 && inRange (a2, b2) b1

overlaps :: ( Ix a) =>Range a -> Range a -> Bool
Range a1 b1 `overlaps` Range a2 b2 = inRange (a2, b2) a1 || inRange (a2, b2) b1

