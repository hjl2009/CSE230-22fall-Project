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
import Data.Char (toUpper)
import Control.Monad.IO.Class (MonadIO(liftIO))

type Name = ()

data Gameplay = Gameplay
    { _game :: Game
    , _title :: String
    }
makeLenses ''Gameplay

app :: App Gameplay e Name
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
playGame = void $ defaultMain app $ Gameplay initGame "a"

drawUI :: Gameplay -> [Widget Name]
drawUI g = [C.center $ drawBoard g]

drawBoard :: Gameplay -> Widget Name
drawBoard g = B.borderWithLabel (str $ show $ g^.game.currentBlock)
    $ vBox $ [boardSize - 1, boardSize - 2 .. 0] <&> \y ->
        hBox $ [0 .. boardSize - 1] <&> \x ->
            let (w, f) = case g^.game.currentBlock of
                    Nothing -> ("  ", Def)
                    Just k -> (if elem (x, y) $ blockTiles k then "[]" else "  ", _player k)
            in toAttr f (fromMaybe Def $ M.lookup (x, y) $ g^.game.board) `withAttr` str w

toAttr :: Player -> Player -> AttrName
toAttr f b = attrName (show f ++ show b)

appEvent :: BrickEvent Name e -> EventM Name Gameplay ()
appEvent (VtyEvent e) = case e of
    V.EvKey V.KEsc [] -> halt
    V.EvKey V.KEnter [] -> do
        g <- use game
        s <- suspendAndResume' getLine
        let p = currentPlayer g
        let mpm = readMaybe $ map toUpper s
        case mpm of
            Nothing -> return ()
            Just pm -> when (available g p pm) $ game.currentBlock .= Just (polyToBlock g pm)
    V.EvKey (V.KChar '[') [] -> use game >>= \g -> game.currentBlock .= genPredBlock g
    V.EvKey (V.KChar ']') [] -> use game >>= \g -> game.currentBlock .= genSuccBlock g
    V.EvKey V.KUp [] -> move UU
    V.EvKey V.KDown [] -> move DD
    V.EvKey V.KLeft [] -> move LL
    V.EvKey V.KRight [] -> move RR
    V.EvKey (V.KChar ' ') [] -> do
        g <- use game
        case _currentBlock g of
            Nothing -> return ()
            Just k -> when (isValidBlock (_board g) k) $ game.board %= placeBlock k >> game.history %= (k :) >> nextPlayer
    V.EvKey (V.KChar 'p') [] -> nextPlayer
    V.EvKey (V.KChar 'f') [] -> rotate $ D8 0 1
    V.EvKey (V.KChar 'd') [] -> rotate $ D8 2 1
    V.EvKey (V.KChar 'r') [] -> rotate $ D8 3 0
    V.EvKey (V.KChar 'e') [] -> rotate $ D8 1 0
    V.EvKey (V.KChar 's') [] -> get >>= \g -> liftIO $ writeFile (_title g ++ ".blokell") $ show $ _game g
    _ -> return ()
appEvent _ = return ()



nextPlayer :: EventM Name Gameplay ()
nextPlayer = (game.currentIndex %= \i -> mod (i + 1) 4) >> game.currentBlock .= Nothing

move :: Dir -> EventM Name Gameplay ()
move d = do
    kk <- use $ game.currentBlock
    case kk of
        Nothing -> return ()
        Just k -> do
            let k' = (center %~ trans d) k
            when ((if isBlockInbound k then isBlockInbound else isBlockPartialInbound) k') $ game.currentBlock .= Just k'

rotate :: D8 -> EventM Name Gameplay ()
rotate d = game.currentBlock._Just.direction %= compose' d


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