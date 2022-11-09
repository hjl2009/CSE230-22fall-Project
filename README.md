# CSE230-22fall-Project
## Basic Information

Team members: Heran Wang, Jiale Huang, Yizhao Zhang, Yulin Luo
Language: Haskell
Library: brick

## Project Goal

In this project, we will implement a famous board game called Blokus, where players try to score points by occupying most of the board with pieces of their color. The board is a square regular grid, and the pieces are polyominoes. 

Specifically, the players play the game according to the following simple rules:
- Each new piece placed on the board must touch a piece with the same color with only corner-to-corner contact.
- No edge-to-edge contact between the pieces with the same color is allowed.
- The game ends when no additional pieces from either player can be placed.
- The score of a player is determined by the total number of squares occupied by the pieces of the corresponding color.

For more details, please check [wiki](https://en.wikipedia.org/wiki/Blokus)

Our main goal is to implement the two-player version of the Blokus game, which supports all the basic gameplay functionalities.

## Details

The functions we will implement are listed below:

1 The basic logic of the game.
2 Save & Load
3 Multiplayer mode
4 Single player mode
5 Simple AI?
