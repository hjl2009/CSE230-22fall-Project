{-# LANGUAGE TemplateHaskell #-}
module UI.Game
    ( Opts(..)
    , playGame
    ) where

import Blokus
import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core (str, clickable, padLeftRight, padTopBottom,
 withDefAttr, translateBy, padBottom, (<=>), setAvailableSize,
 padLeft, padRight, padTop, (<+>))
import Brick.Types as T 
import Control.Monad (void, when)
import qualified Data.Map as M
import qualified Graphics.Vty as V
import Lens.Micro.Platform
import Text.Read (readMaybe)
import Data.Char (toUpper)
import Data.List (isSuffixOf, sortOn)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Control.Monad.IO.Class (MonadIO(liftIO))

type Name = ()
 

data Gameplay = Gameplay
    { _game :: Game
    , _filename :: String
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
    }

initGame :: Game
initGame = Game (M.fromList $ zip outerCorners [Red, Green, Blue, Yellow]) [] 0 Nothing

extension :: String
extension = ".blokell"

toFileName :: String -> String
toFileName s = if extension `isSuffixOf` s then s else s ++ extension

playGame :: Opts -> IO ()
playGame o = do
    let Opts l s = o
    g <- case l of
        Just l' -> readFile (toFileName l') <&> readMaybe
        Nothing -> return Nothing
    s' <- ("game" ++ ) . iso8601Show <$> getCurrentTime
    void $ defaultMain app $ Gameplay (fromMaybe initGame g) $ toFileName $ fromMaybe s' s

drawUI :: Gameplay -> [Widget Name]
drawUI g = [C.center $ drawStats (_game g) <+> drawBoard g <+> buttonLayer g]

drawBoard :: Gameplay -> Widget Name
drawBoard g = B.borderWithLabel (str $ show $ g^.game.currentBlock)
    $ vBox $ [boardSize - 1, boardSize - 2 .. 0] <&> \y ->
        hBox $ [0 .. boardSize - 1] <&> \x ->
            let (w, f) = case g^.game.currentBlock of
                    Nothing -> ("  ", Def)
                    Just k -> (if elem (x, y) $ blockTiles k then "[]" else "  ", _player k)
            in toAttr f (fromMaybe Def $ M.lookup (x, y) $ g^.game.board) `withAttr` str w

drawStats :: Game -> Widget Name
drawStats g = B.border . setAvailableSize (13, 9) . padTop (Pad 1) . vBox . map (uncurry (drawStat' pp)) . computeStats $ g
    where pp = currentPlayer g

drawFinalStats :: Game -> Widget Name
drawFinalStats = B.border . setAvailableSize (13, 9) . padTop (Pad 1) . vBox . map (uncurry drawStat) . sortOn (negate . snd) . reverse . computeStats

buttonLayer :: Gameplay -> Widget Name
buttonLayer st =
    C.vCenterLayer $
      C.hCenterLayer (padBottom (Pad 1) $ str "[ : Prev Block") <=>
      C.hCenterLayer (padBottom (Pad 1) $ str "] : Next Block") <=>
      C.hCenterLayer (padBottom (Pad 1) $ str "E/R : rotate Block") <=>
      C.hCenterLayer (padBottom (Pad 1) $ str "D/F : overturn Block") <=>
      C.hCenterLayer (padBottom (Pad 1) $ str "space : Place Block") <=>
      C.hCenterLayer (padBottom (Pad 1) $ str "up/down/right/left : Move Block") <=>
      C.hCenterLayer (padBottom (Pad 1) $ str "esc : Exit Game")



computeStats :: Game -> [(Player, Int)]
computeStats g = map (\x -> g^.board.at x.non Def) outerCorners <&> \p -> (p, sum [length $ polyominoTiles pm | Block pm _ _ p' <- _history g, p == p'])

drawInnerStat :: (Widget n1 -> Widget n2) -> Player -> Int -> Widget n2
drawInnerStat f p x = f (toAttr Def p `withAttr` str "  ") <+> padLeft Max (padRight (Pad 2) $ str $ show x)

drawStat :: Player -> Int -> Widget Name
drawStat p x = padBottom (Pad 1) $ drawInnerStat (padLeftRight 2) p x

drawStat' :: Player -> Player -> Int -> Widget Name
drawStat' p' p x = padBottom (Pad 1) $ padLeftRight 2 (str $ if p == p' then "o" else " ") <+> drawInnerStat (padRight $ Pad 2) p x

toAttr :: Player -> Player -> AttrName
toAttr f b = attrName (show f ++ show b)

appEvent :: BrickEvent Name e -> EventM Name Gameplay ()
appEvent (VtyEvent e) = case e of
    V.EvKey V.KEsc [] -> halt
    V.EvKey (V.KChar 'q') [] -> halt
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
    V.EvKey (V.KChar 's') [] -> get >>= \g -> liftIO $ writeFile (_filename g) $ show $ _game g
    _ -> return ()
appEvent _ = return ()

nextPlayer :: EventM Name Gameplay ()
nextPlayer = (game.currentIndex %= \i -> mod (i + 1) 4) >> game.currentBlock .= Nothing

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