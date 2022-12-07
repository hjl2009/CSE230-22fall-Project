{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
import Blokus
import Blokus.AI
import Test.QuickCheck
import Lens.Micro.Platform

main :: IO ()
main = do
    quickCheck prop_inbound
    quickCheck $ prop_isValid simpleAI

instance Arbitrary D8 where
    arbitrary :: Gen D8
    arbitrary = D8 <$> chooseInt (0, 3) <*> chooseInt (0, 1)

instance Arbitrary Block where
    arbitrary :: Gen Block
    arbitrary = Block
        <$> chooseEnum (I1, Z)
        <*> (pair <$> chooseInt (0, boardSize - 1) <*> chooseInt (0, boardSize - 1))
        <*> arbitrary
        <*> chooseEnum (Blue, Yellow)
        where pair x y = (x, y)

isBlockInbound :: Block -> Bool
isBlockInbound k = all isPosInbound $ blockTiles k

prop_inbound :: Block -> Bool
prop_inbound k = isBlockInbound $ moveBlockInbound k

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes n f x
    | n <= 0 = x
    | otherwise = f $ applyNTimes (n - 1) f x

makeMove :: MyAI -> Game -> Game
makeMove i g = (currentIndex %~ \i -> mod (i + 1) 4) $ case runAI i g of
    Nothing -> g
    Just k -> (board %~ placeBlock k) $ (history %~ (k :)) g

genGame :: Gen Game
genGame = do
    n <- chooseInt (40, 60)
    return $ applyNTimes n (makeMove simpleAI) initGame

isValid :: Game -> Maybe Block -> Bool
isValid g = maybe True (\k -> isValidBlock (_board g) k && available g (currentPlayer g) (_shape k))

prop_isValid :: MyAI -> Property
prop_isValid i = forAll genGame $ \g -> isValid g (runAI i g)