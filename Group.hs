module Group where

class Monoid g => Group g where
  (><) :: g -> g -> g
  
  inv :: g -> g
  inv = (mempty ><)