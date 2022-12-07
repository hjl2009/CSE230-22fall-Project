{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Blokus.AI
    ( StatelessDeterministicAI(..)
    , MyAI
    , simpleAI
    , naiveAI
    , hintAI
    ) where

import Blokus
import qualified Data.Array as A
import qualified Data.Map as M
import qualified Data.Set as S

class StatelessDeterministicAI g m a | a -> g m where
    runAI :: a -> g -> m

data GameInfo = GameInfo
    { _iColors :: A.Array V2 Player
    , _iPlayer :: Player
    , _iPieces :: S.Set Polyomino
    , _iRange :: (V2, V2)
    , _iCount :: Int
    }

type MyAI = Game -> GameInfo -> Maybe Block

instance StatelessDeterministicAI Game (Maybe Block) MyAI where
    runAI :: MyAI -> Game -> Maybe Block
    runAI a g = a g $ infoGame g

zeroAI :: MyAI
zeroAI _ _ = Nothing

updateInterval :: Int -> V2 -> V2
updateInterval x (l, r) = (min l x, max r x)

updateRange :: V2 -> (V2, V2) -> (V2, V2)
updateRange (x, y) (l, r) = (updateInterval x l, updateInterval y r)

combineRange :: (V2, V2) -> (V2, V2) -> (V2, V2)
combineRange ((x1, x2), (y1, y2)) = updateRange (x1, y1) . updateRange (x2, y2)

rangeH :: (V2, V2) -> Int
rangeH ((x1, x2), (y1, y2)) = x + y + min x y
    where
        x = x2 - x1
        y = y2 - y1

emptyInterval :: (Int, Int)
emptyInterval = (boardSize + 1, -2)

emptyRange :: (V2, V2)
emptyRange = (emptyInterval, emptyInterval)

rangeBlock :: Block -> (V2, V2)
rangeBlock k = foldr updateRange emptyRange $ blockTiles k

infoGame :: Game -> GameInfo
infoGame g = GameInfo a p (S.fromList $ filter (available g p) polyominoes) r c
    where
        p = currentPlayer g
        b = _board g
        a = A.array ((-1, -1), (boardSize, boardSize))
            [ ((x, y), M.findWithDefault Def (x, y) b)
            | x <- [-1 .. boardSize]
            , y <- [-1 .. boardSize]
            ]
        r = M.foldlWithKey' (\rr x _ -> updateRange x rr) emptyRange $ M.filter (== p) b
        c = countValid p a

countValid :: Player -> A.Array V2 Player -> Int
countValid p a = sum
    [ if predicate (x, y) then 1 else 0
    | x <- [0 .. boardSize - 1]
    , y <- [0 .. boardSize - 1]
    ]
    where predicate x = free && good && not bad
            where
                free = Def == a A.! x
                good = p `elem` [a A.! add x y | y <- [(1, 1), (1, -1), (-1, 1), (-1, -1)]]
                bad = p `elem` [a A.! add x y | y <- [(-1, 0), (1, 0), (0, -1), (0, 1)]]

countH :: Player -> A.Array V2 Player -> Block -> Int
countH p a k = countValid p $ a A.// [(x, p) | x <- blockTiles k]

heuristics :: Game -> GameInfo -> Block -> Int
heuristics g i k = rangeH (combineRange r $ rangeBlock k) - rangeH r + countH (_iPlayer i) (_iColors i) k - _iCount i
    where r = _iRange i

chooseH :: (Ord k) => (Block -> k) -> Block -> Block -> Block
chooseH h a b = if h b > h a then b else a

composeSeq :: MyAI -> MyAI -> MyAI
composeSeq a b g i = case a g i of
    Just ma -> Just ma
    Nothing -> b g i

composeWith :: (Block -> Block -> Block) -> MyAI -> MyAI -> MyAI
composeWith f a b g i = case a g i of
    Nothing -> b g i
    Just ma -> case b g i of
        Nothing -> Just ma
        Just mb -> Just $ f ma mb

leveledGreedyAITemplate :: (MyAI -> MyAI -> MyAI) -> MyAI
leveledGreedyAITemplate c g i = foldr (composeSeq . levelAI) zeroAI [5, 4 .. 1] g i
    where levelAI = foldr (c . polyominoAITemplate c) zeroAI . filter (`S.member` _iPieces i) . polyominoesSized

polyominoesSized :: Int -> [Polyomino]
polyominoesSized 5 = [F .. Z]
polyominoesSized 4 = [I4 .. T4]
polyominoesSized 3 = [I3, L3]
polyominoesSized 2 = [I2]
polyominoesSized 1 = [I1]
polyominoesSized _ = []

polyominoAITemplate :: (MyAI -> MyAI -> MyAI) -> Polyomino -> MyAI
polyominoAITemplate c pm g i = (foldr c zeroAI $
    [ checkBlockAI $ Block pm (x, y) d $ _iPlayer i
    | x <- [0 .. boardSize - 1]
    , y <- [0 .. boardSize - 1]
    , d <- dirs $ sym pm
    ]) g i

isValidBlockInfo :: Game -> GameInfo -> Block -> Bool
isValidBlockInfo _ i k = all predicate t && any good t
    where
        t = blockTiles k
        p = _player k
        a = _iColors i
        good x = p `elem` [a A.! add x y | y <- [(1, 1), (1, -1), (-1, 1), (-1, -1)]]
        predicate x = isPosInbound x && free && not bad
            where
                free = Def == a A.! x
                bad = p `elem` [a A.! add x y | y <- [(-1, 0), (1, 0), (0, -1), (0, 1)]]


checkBlockAI :: Block -> MyAI
checkBlockAI k g i = if isValidBlockInfo g i k then Just k else Nothing

data Sym = Ig | C2g | T2g | K4g | D8g

sym :: Polyomino -> Sym

sym I1 = D8g
sym I2 = K4g
sym I3 = K4g
sym L3 = T2g
sym I4 = K4g
sym N4 = C2g
sym O4 = D8g
sym T4 = T2g
sym I = K4g
sym T = T2g
sym U = T2g
sym V = T2g
sym W = T2g
sym X = D8g
sym Z = C2g
sym _ = Ig

dirs :: Sym -> [D8]
dirs Ig = [D8 s t | s <- [0 .. 3], t <- [0, 1]]
dirs C2g = [D8 s t | s <- [0, 1], t <- [0, 1]]
dirs T2g = [D8 s 0 | s <- [0 .. 3]]
dirs K4g = [D8 s 0 | s <- [0, 1]]
dirs D8g = [D8 0 0]

simpleAI :: MyAI
simpleAI = leveledGreedyAITemplate composeSeq

naiveAI :: MyAI
naiveAI g i = (leveledGreedyAITemplate $ composeWith $ chooseH $ heuristics g i) g i

hintAI :: Polyomino -> MyAI
hintAI = polyominoAITemplate composeSeq
