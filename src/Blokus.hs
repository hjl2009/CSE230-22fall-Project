{-# LANGUAGE TemplateHaskell #-}
module Blokus
    ( Polyomino
    , polyominoes
    , polyominoTiles
    , D8
    , compose
    , act
    , Player
    , Block
    ) where
import Lens.Micro.Platform
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

boardSize :: Int
boardSize = 20

-- | Vector of two integers
type V2 = (Int, Int)

-- | Polyomino names
data Polyomino = I1 | I2 | I3 | L3 | I4 | L4 | N4 | O4 | T4 | F | I | L | N | P | T | U | V | W | X | Y | Z
    deriving (Eq, Show, Read)

polyominoes :: [Polyomino]
polyominoes = [I1, I2, I3, L3, I4, L4, N4, O4, T4, F, I, L, N, P, T, U, V, W, X, Y, Z]

-- | Tiles of a polyomino
polyominoTiles :: Polyomino -> [V2]
polyominoTiles I1 = [(0, 0)]
polyominoTiles I2 = [(0, 0), (1, 0)]
polyominoTiles I3 = [(-1, 0), (0, 0), (1, 0)]
polyominoTiles L3 = [(0, 0), (1, 0), (0, 1)]
polyominoTiles I4 = [(-1, 0), (0, 0), (1, 0), (2, 0)]
polyominoTiles L4 = [(-1, 0), (0, 0), (1, 0), (1, 1)]
polyominoTiles N4 = [(0, 0), (1, 0), (-1, 1), (0, 1)]
polyominoTiles O4 = [(0, 0), (1, 0), (0, 1), (1, 1)]
polyominoTiles T4 = [(-1, 0), (0, 0), (1, 0), (0, 1)]
polyominoTiles F = [(0, -1), (-1, 0), (0, 0), (1, 0), (1, 1)]
polyominoTiles I = [(-2, 0), (-1, 0), (0, 0), (1, 0), (2, 0)]
polyominoTiles L = [(-2, 0), (-1, 0), (0, 0), (1, 0), (1, 1)]
polyominoTiles N = [(-2, 0), (-1, 0), (0, 0), (0, 1), (1, 1)]
polyominoTiles P = [(-1, 0), (0, 0), (1, 0), (0, 1), (1, 1)]
polyominoTiles T = [(-1, 0), (0, 0), (1, 0), (0, 1), (0, 2)]
polyominoTiles U = [(-1, 0), (0, 0), (1, 0), (-1, 1), (1, 1)]
polyominoTiles V = [(-1, -1), (0, -1), (1, -1), (1, 0), (1, 1)]
polyominoTiles W = [(-1, -1), (0, -1), (0, 0), (1, 0), (1, 1)]
polyominoTiles X = [(0, -1), (-1, -1), (0, 0), (1, 0), (0, 1)]
polyominoTiles Y = [(-2, 0), (-1, 0), (0, 0), (1, 0), (0, 1)]
polyominoTiles Z = [(0, -1), (1, -1), (0, 0), (-1, 1), (0, 1)]


-- | Dihedral group for rotation
data D8 = D8 Int Int
    deriving (Eq, Show)

-- | Composition of two D8 group elements
compose :: D8 -> D8 -> D8
compose (D8 s1 0) (D8 s2 t2) = D8 (mod (s1 + s2) 4) t2
compose (D8 s1 1) (D8 s2 t2) = D8 (mod (s1 - s2) 4) (1 - t2)
compose (D8 s1 t1) x = compose (D8 s1 $ mod t1 2) x

-- | D8 acting on a coordinate
act :: D8 -> V2 -> V2
act (D8 0 0) z = z
act (D8 1 0) (x, y) = (-y, x)
act (D8 2 0) (x, y) = (-x, -y)
act (D8 3 0) (x, y) = (y, -x)
act (D8 s 1) z = (-x, y) where (x, y) = act (D8 s 0) z
act (D8 s t) z = act (D8 (mod s 4) (mod t 2)) z

-- | Player (color)
data Player = Red | Blue | Green | Yellow
    deriving (Eq, Show)

-- | Block, a instantiated polyomino
data Block = Block
    { _shape :: Polyomino
    , _center :: V2
    , _direction :: D8
    , _player :: Player
    } deriving (Eq, Show)
makeLenses ''Block

type Board = Map.Map V2 Player

data Game = Game
    { _board :: Board
    , _placed :: [Block]
    , _currentPlayer :: Int
    , _currentBlock :: Maybe Block
    }
makeLenses ''Game

blockTiles :: Block -> [V2]
blockTiles k = map (act $ _direction k) $ polyominoTiles $ _shape k

isPosFree :: Board -> V2 -> Bool
isPosFree b x = not $ Map.member x b

isPosGood :: Board -> Player -> V2 -> Bool
isPosGood b p x = any adj [add x y | y <- [(1, 1), (1, -1), (-1, 1), (-1, -1)]]
    where adj y = fromMaybe False $ Map.lookup y b >>= return . (p == )

isPosInbound :: V2 -> Bool
isPosInbound (x, y) = 0 <= x && x < boardSize && 0 <= y && y < boardSize

isBlockInbound :: Block -> Bool
isBlockInbound k = all isPosInbound $ blockTiles k

isValidBlock :: Board -> Block -> Bool
isValidBlock b k = all (isPosFree b) t && any (isPosGood b $ _player k) t && all isPosInbound t
    where t = blockTiles k


placeBlock :: Board -> Block -> Board
placeBlock b k = Map.union b $ Map.fromList [(x, _player k) | x <- blockTiles k]

data Dir = UU | DD | LL | RR
    deriving (Eq, Show)

add :: V2 -> V2 -> V2
add (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)

trans :: Dir -> V2 -> V2
trans UU = add (0, 1)
trans DD = add (0, -1)
trans LL = add (-1, 0)
trans RR = add (1, 0)