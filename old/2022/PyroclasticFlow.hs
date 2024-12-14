module PyroclasticFlow where
import Control.Monad.State
import Data.Word (Word8)
import Data.Bits
import Data.Bool
import Data.These
import Data.Align
import Data.Foldable

data GustOfHotGas = BlowLeft | BlowRight
data Rock = Horizontal | Plus | Elbow | Vertical | Square
type Row = Word8
data TetrisRegion = TetrisRegion { rows :: [Row], offset :: Int }

instance Show TetrisRegion where
    show (TetrisRegion rows offset) = unlines $
        ("y = " <> show offset) : map showRow rows
        where
            showRow :: Row -> String
            showRow row = bool '.' '@' . testBit row <$> reverse [0..6]

tetrisRegion :: [Word8] -> TetrisRegion
tetrisRegion rows = TetrisRegion rows (length rows -1)

newGame :: TetrisRegion
newGame = tetrisRegion [127]

rockRegion :: Rock -> TetrisRegion
rockRegion = \case
    Horizontal -> tetrisRegion [30]
    Plus -> tetrisRegion [8, 28, 8]
    Elbow -> tetrisRegion [4, 4, 28]
    Vertical -> tetrisRegion [16, 16, 16, 16]
    Square -> tetrisRegion [24, 24]

union :: TetrisRegion -> TetrisRegion -> TetrisRegion
TetrisRegion [] _ `union` tr = tr
tr `union` TetrisRegion [] _ = tr
TetrisRegion rs1 off1 `union` TetrisRegion rs2 off2 =
    let rs1' = replicate (max 0 (off1 - off2)) 0 <> rs1
        rs2' = replicate (max 0 (off2 - off1)) 0 <> rs2
    in TetrisRegion (alignWith (mergeThese (.|.)) rs1' rs2') (max off1 off2)

intersect :: TetrisRegion -> TetrisRegion -> TetrisRegion
TetrisRegion [] _ `intersect` _ = TetrisRegion [] 0
_ `intersect` TetrisRegion [] _ = TetrisRegion [] 0
TetrisRegion rs1 off1 `intersect` TetrisRegion rs2 off2 =
    let rs1' = drop (max 0 (off1 - off2)) rs1
        rs2' = drop (max 0 (off2 - off1)) rs2
    in  TetrisRegion (zipWith (.&.) rs1' rs2') (min off1 off2)

overlaps :: TetrisRegion -> TetrisRegion -> Bool
overlaps tr1 tr2 =
    let TetrisRegion rs _ = intersect tr1 tr2
    in  all (==0) rs

gust :: TetrisRegion -> GustOfHotGas -> TetrisRegion
gust (TetrisRegion rows offset) gustDirection =
    let range = foldr (.|.) 0 rows
        n = case gustDirection of
            BlowLeft  -> if not (testBit range 6) then 1  else 0
            BlowRight -> if not (testBit range 0) then -1 else 0
    in TetrisRegion (map (`shift` n) rows) offset

pop :: (MonadState [a] m) => m a
pop = gets head <* modify tail

dropRock :: TetrisRegion -> Rock -> TetrisRegion
dropRock r@(TetrisRegion rows offset) rock =
    let TetrisRegion rockRows rockOffset = rockRegion rock
    in r `union` TetrisRegion rockRows (offset + rockOffset + 3)


day17 :: IO ()
day17 = do
    let r = TetrisRegion [8] 0
    let s = TetrisRegion [8] 1
    --print (r `union` s)
    --print (s `union` r)
    --print (rockRegion Vertical)
    print (newGame `dropRock` Elbow)
    traverse_ (print . rockRegion) [Vertical, Horizontal, Square, Elbow, Plus]
