{-# LANGUAGE TemplateHaskell #-}
module UI.Game
    ( Opts(..)
    , playGame
    ) where

import Blokus
import Blokus.AI
import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Control.Monad (void, when)
import qualified Data.Map as M
import qualified Graphics.Vty as V
import Lens.Micro.Platform
import Text.Read (readMaybe)
import Data.Char (toUpper)
import Data.Foldable (forM_)
import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe, isNothing)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Control.Monad.IO.Class (MonadIO(liftIO))

type Name = ()

data Gameplay = Gameplay
    { _game :: Game
    , _filename :: String
    , _passCount :: Int
    , _noCheckPass :: Bool
    , _hint :: Bool
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

data Opts = Opts
    { _loadFilename :: Maybe String
    , _saveFilename :: Maybe String
    , _checkedPass :: Bool
    , _enableHint :: Bool
    }

extension :: String
extension = ".blokell"

toFileName :: String -> String
toFileName s = if extension `isSuffixOf` s then s else s ++ extension

playGame :: Opts -> IO ()
playGame o = do
    let Opts l s c h = o
    g <- case l of
        Just l' -> readFile (toFileName l') <&> readMaybe
        Nothing -> return Nothing
    s' <- ("game" ++ ) . iso8601Show <$> getCurrentTime
    void $ defaultMain app $ Gameplay (fromMaybe initGame g) (toFileName $ fromMaybe s' s) 0 (not c) h

isOver :: Gameplay -> Bool
isOver g = _passCount g == 4

drawUI :: Gameplay -> [Widget Name]
drawUI g = [C.center $ (drawLeft g <+> drawBoard g) <=> drawPieces g]

drawLeft :: Gameplay -> Widget Name
drawLeft g = setAvailableSize (15, 22) $ drawGameOver (isOver g) <=> drawStats (_game g)

drawBoard :: Gameplay -> Widget Name
drawBoard g = B.border $ vBox
    $ [boardSize - 1, boardSize - 2 .. 0] <&> \y ->
        hBox $ [0 .. boardSize - 1] <&> \x ->
            let (w, f) = case g^.game.currentBlock of
                    Nothing -> ("  ", Def)
                    Just k -> (if elem (x, y) $ blockTiles k then "[]" else "  ", _player k)
            in toAttr f (fromMaybe Def $ M.lookup (x, y) $ g^.game.board) `withAttr` str w

drawPieces :: Gameplay -> Widget Name
drawPieces g = B.border $ setAvailableSize (55, 1) $ C.center $ padLeft (Pad 1) $ hBox
    $ polyominoes <&> \pm ->
        str ((if available (_game g) p pm then id else map $ const ' ') $ show pm ++ " ")
        & maybe id (\k -> if pm == _shape k then withAttr (toAttr p Def) else id) (g^.game.currentBlock)
    where p = currentPlayer $ _game g

drawStats :: Game -> Widget Name
drawStats g = B.border . padTop (Pad 1) . vBox . map (padBottom (Pad 1) . uncurry (drawStat pp)) . computeStats $ g
    where pp = currentPlayer g

computeStats :: Game -> [(Player, Int)]
computeStats g = map (\x -> g^.board.at x.non Def) outerCorners <&> \p -> (p, sum [length $ polyominoTiles pm | Block pm _ _ p' <- _history g, p == p'])

drawInnerStat :: (Widget n1 -> Widget n2) -> Player -> Int -> Widget n2
drawInnerStat f p x = f (toAttr Def p `withAttr` str "  ") <+> padLeft Max (padRight (Pad 2) $ str $ show x)

drawStat :: Player -> Player -> Int -> Widget Name
drawStat p' p x =  padLeftRight 2 (str $ if p == p' then "o" else " ") <+> drawInnerStat (padRight $ Pad 2) p x

drawGameOver :: Bool -> Widget Name
drawGameOver x = C.center $ if x then str "GAME OVER" else str " "

toAttr :: Player -> Player -> AttrName
toAttr f b = attrName (show f ++ show b)

appEvent :: BrickEvent Name e -> EventM Name Gameplay ()
appEvent x@(VtyEvent e) = case e of
    V.EvKey V.KEsc [] -> halt
    V.EvKey (V.KChar 'q') [] -> halt
    V.EvKey (V.KChar 'c') [V.MCtrl] -> halt
    _ -> use passCount >>= \c -> when (c < 4) $ appEventInner x
appEvent _ = return ()

appEventInner :: BrickEvent Name e -> EventM Name Gameplay ()
appEventInner (VtyEvent e) = case e of
    V.EvKey V.KEnter [] -> do
        g <- use game
        s <- suspendAndResume' getLine
        let p = currentPlayer g
        let mpm = readMaybe $ map toUpper s
        case mpm of
            Nothing -> return ()
            Just pm -> when (available g p pm) $ game.currentBlock .= Just (polyToBlock g pm)
    V.EvKey (V.KChar 'z') [] -> use game >>= \g -> game.currentBlock .= genPredBlock g
    V.EvKey (V.KChar 'x') [] -> use game >>= \g -> game.currentBlock .= genSuccBlock g
    V.EvKey V.KUp [] -> move UU
    V.EvKey V.KDown [] -> move DD
    V.EvKey V.KLeft [] -> move LL
    V.EvKey V.KRight [] -> move RR
    V.EvKey (V.KChar ' ') [] -> do
        g <- use game
        case _currentBlock g of
            Nothing -> return ()
            Just k -> when (isValidBlock (_board g) k) $ applyBlock k >> pass
    V.EvKey (V.KChar 'p') [] -> get >>= \g -> when (_noCheckPass g || isNothing (runAI simpleAI $ _game g)) pass
    V.EvKey (V.KChar 'f') [] -> rotate $ D8 0 1
    V.EvKey (V.KChar 'd') [] -> rotate $ D8 2 1
    V.EvKey (V.KChar 'r') [] -> rotate $ D8 3 0
    V.EvKey (V.KChar 'e') [] -> rotate $ D8 1 0
    V.EvKey (V.KChar 's') [] -> get >>= \g -> liftIO $ writeFile (_filename g) $ show $ _game g
    V.EvKey (V.KChar 'i') [] -> do
        g <- use game
        forM_ (runAI simpleAI g) applyBlock
        pass
    V.EvKey (V.KChar 'j') [] -> do
        g <- use game
        forM_ (runAI naiveAI g) applyBlock
        pass
    V.EvKey (V.KChar 'h') [] -> use hint >>= \h -> when h $ do
        g <- use game
        case _currentBlock g of
            Nothing -> return ()
            Just k -> game.currentBlock._Just .= fromMaybe k (runAI (hintAI $ _shape k) g)
    _ -> return ()
appEventInner _ = return ()

applyBlock :: Block -> EventM Name Gameplay ()
applyBlock k = (passCount .= -1) >> game . board %= placeBlock k >> game . history %= (k :)

pass :: EventM Name Gameplay ()
pass = passCount += 1 >> (game . currentIndex %= \i -> mod (i + 1) 4) >> game . currentBlock .= Nothing

move :: Dir -> EventM Name Gameplay ()
move d = game.currentBlock._Just %= moveBlockInbound . (center %~ trans d)

rotate :: D8 -> EventM Name Gameplay ()
rotate d = game.currentBlock._Just %= moveBlockInbound . (direction %~ compose' d)


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
