import Data.Array

-- match, gap in the first sequence, gap in the second sequence
data Alignment = Match | GapA | GapB deriving (Show, Eq, Ord)

-- a single cell in the score/path matrix
data MCell = MC { score :: Int
                , align :: Alignment
                } deriving (Eq, Show)

instance Ord MCell where
  compare (MC s _) (MC t _) = compare s t -- don't compare alignment


sim a b
  | a == b    = 1
  | otherwise = 0

gap = -1

nw x' y' = let b = bt lx ly in (reverse $ fst b, reverse $ snd b) where
  (lx, ly) = (length x', length y')
  ( x,  y) = (listArray (1, lx) x', listArray (1, ly) y')
  a = listArray bounds [f i j | (i, j) <- range bounds]
  bounds = ((0, 0), (lx, ly)) 
  f 0 j = MC (-j) GapA
  f i 0 = MC (-i) GapB
  f i j = maximum [ MC (score (a ! (i-1, j-1)) + sim (x!i) (y!j)) Match
                  , MC (score (a ! (i-1, j-0)) + gap)             GapB
                  , MC (score (a ! (i-0, j-1)) + gap)             GapA ]
  bt 0 0 = ("", "") -- backtracking 
  bt i j = case a ! (i, j) of
    (MC _ Match) -> let (px, py) = bt (i-1) (j-1) in ((x!i):px, (y!j):py)
    (MC _ GapA)  -> let (px, py) = bt (i-0) (j-1) in (  '-':px, (y!j):py)
    (MC _ GapB)  -> let (px, py) = bt (i-1) (j-0) in ((x!i):px,   '-':py)

-- compare insulin

test = nw "AGCCCTCCAGGACAGGCTGCATCAGAAGAGGCCATCAAGCAGGTCTGTTCCAAGGGCCTTTGCGTCAGATCACTGTCCTTCTGCCATGGCCCTGTGGATGCGCCTCCTGCCCCTGCTGGCGCTGCTGGCCCTCTGGGGACCTGACCCAGCCGCAGCCTTTGTGAACCAACACCTGTGCGGCTCACACCTGGTGGAAGCTCTCTACCTAGTGTGCGGGGAACGAGGCTTCTTCTACACACCCAAGACCCGCCGGGAGGCAGAGGACCTGCAGGTGGGGCAGGTGGAGCTGGGCGGGGGCCCTGGTGCAGGCAGCCTGCAGCCCTTGGCCCTGGAGGGGTCCCTGCAGAAGCGTGGCATTGTGGAACAATGCTGTACCAGCATCTGCTCCCTCTACCAGCTGGAGAACTACTGCAACTAGACGCAGCCCGCAGGCAGCCCCACACCCGCCGCCTCCTGCACCGAGAGAGATGGAATAAAGCCCTTGAACCAGCAAAA" "AGCCCTCCAGGACAGGCTGCATCAGAAGAGGCCATCAAGCAGATCACTGTCCTTCTGCCATGGCCCTGTGGATGCGCCTCCTGCCCCTGCTGGCGCTGCTGGCCCTCTGGGGACCTGACCCAGCCGCAGCCTTTGTGAACCAACACCTGTGCGGCTCACACCTGGTGGAAGCTCTCTACCTAGTGTGCGGGGAACGAGGCTTCTTCTACACACCCAAGACCCGCCGGGAGGCAGAGGACCTGCAGGTGGGGCAGGTGGAGCTGGGCGGGGGCCCTGGTGCAGGCAGCCTGCAGCCCTTGGCCCTGGAGGGGTCCCTGCAGAAGCGTGGCATTGTGGAACAATGCTGTACCAGCATCTGCTCCCTCTACCAGCTGGAGAACTACTGCAACTAGACGCAGCCCGCAGGCAGCCCCACACCCGCCGCCTCCTGCACCGAGAGAGATGGAATAAAGCCCTTGAACCAGCAAAA"
