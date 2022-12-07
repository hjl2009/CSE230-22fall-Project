module Main (main) where

import UI.Game
import Options.Applicative

opts :: Parser Opts
opts = Opts
    <$> optional (strOption
        (  long "load"
        <> short 'l'
        ))
    <*> optional (strOption
        (  long "name"
        <> short 'n'
        ))
    <*> switch
        (  long "pass-no-move"
        <> short 'p'
        )
    <*> switch
        (  long "hint"
        )

fullopts :: ParserInfo Opts
fullopts = info (helper <*> opts)
    (  fullDesc
    <> header "blokell - the Haskell implementation of a famous board game"
    )

main :: IO ()
main = execParser fullopts >>= playGame
