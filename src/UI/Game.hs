{-# LANGUAGE TemplateHaskell #-}
module UI.Game
    ( playGame
    , theMap
    ) where

import Blokus
import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Control.Monad (void, when)
import qualified Data.Map as M
import qualified Graphics.Vty as V
import Lens.Micro.Platform
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

type Name = ()

app :: App Game e Name
app = App
    { appDraw = drawUI
    , appChooseCursor = neverShowCursor
    , appHandleEvent = appEvent
    , appStartEvent = return ()
    , appAttrMap = const theMap
    }

initGame :: Game
-- initGame = Game M.empty [] 0 Nothing
initGame = Game (M.fromList $ zip corners [Red, Green, Blue, Yellow]) [] 0 Nothing

playGame :: IO ()
playGame = void $ defaultMain app initGame

drawUI :: Game -> [Widget Name]
drawUI g = [C.center $ drawBoard g]

drawBoard :: Game -> Widget Name
drawBoard g = B.borderWithLabel (str $ show $ _currentBlock g)
    $ vBox $ [boardSize - 1, boardSize - 2 .. 0] <&> \y ->
        hBox $ [0 .. boardSize - 1] <&> \x ->
            let (w, f) = case _currentBlock g of
                    Nothing -> ("  ", Def)
                    Just k -> (if elem (x, y) $ blockTiles k then "[]" else "  ", _player k)
            in toAttr f (fromMaybe Def $ M.lookup (x, y) $ _board g) `withAttr` str w

toAttr :: Player -> Player -> AttrName
toAttr f b = attrName (show f ++ show b)

appEvent :: BrickEvent Name e -> EventM Name Game ()
appEvent (VtyEvent e) = case e of
    V.EvKey V.KEsc [] -> halt
    V.EvKey V.KEnter [] -> do
        g <- get
        s <- suspendAndResume' getLine
        let p = currentPlayer g
        let mpm = readMaybe s
        case mpm of
            Nothing -> return ()
            Just pm -> when (available g pm p) $ do
                case _currentBlock g of
                    Nothing -> currentBlock .= Just (Block pm (0, 0) (D8 0 0) p)
                    _ -> currentBlock._Just.shape .= pm
    V.EvKey V.KUp [] -> move UU
    V.EvKey V.KDown [] -> move DD
    V.EvKey V.KLeft [] -> move LL
    V.EvKey V.KRight [] -> move RR
    V.EvKey (V.KChar ' ') [] -> do
        g <- get
        case _currentBlock g of
            Nothing -> nextPlayer
            Just k -> when (isValidBlock (_board g) k) $ board %= placeBlock k >> history %= (k :) >> nextPlayer
    V.EvKey (V.KChar 'p') [] -> nextPlayer
    V.EvKey (V.KChar 'f') [] -> rotate $ D8 0 1
    V.EvKey (V.KChar 'd') [] -> rotate $ D8 2 1
    V.EvKey (V.KChar 'r') [] -> rotate $ D8 3 0
    V.EvKey (V.KChar 'e') [] -> rotate $ D8 1 0
    _ -> return ()
appEvent _ = return ()

nextPlayer :: EventM Name Game ()
nextPlayer = (currentIndex %= \i -> mod (i + 1) 4) >> currentBlock .= Nothing

move :: Dir -> EventM Name Game ()
move d = do
    kk <- use currentBlock
    case kk of
        Nothing -> return ()
        Just k -> do
            if isBlockInbound k
                then do
                    let k' = (center %~ trans d) k
                    when (isBlockInbound k') $ currentBlock .= Just k'
                else
                    currentBlock .= Just ((center %~ trans d) k)

rotate :: D8 -> EventM Name Game ()
rotate d = currentBlock._Just.direction %= compose' d


toColor :: Player -> V.Color
toColor Blue = V.blue
toColor Green = V.green
toColor Red = V.red
toColor Yellow = V.yellow
toColor Def = V.black

theMap :: AttrMap
theMap = attrMap V.defAttr $
    [ (toAttr Def p, bg $ toColor p)
    | p <- players
    ] ++
    [ (toAttr p Def, fg $ toColor p)
    | p <- players
    ] ++
    [ (toAttr p q, toColor p `on` toColor q)
    | p <- players
    , q <- players
    ]