> module Game.Chessy.Notation where
>
> import Prelude hiding ((!!))
> import Data.Array ((!))
> import Game.Chessy.Board
> import Debug.Trace
> import Text.Parsec

A primer and syntax definition on algebraic chess notation

First, an example

1. e4 e5
2. Nf3 Nc6
3. Bb5 a6

> firstGame :: Board
> firstGame = game $ do
>   move "e2" "e4" >> move "e7" "e5" -- 1. e4  e5
>   move "g1" "f3" >> move "b8" "c6" -- 2. Nf3 Nc6
>   move "f1" "b5" >> move "a7" "a6" -- 3. Bb5 a6

Let's break these down to the smallest possible pieces

1. e4 e5

This is a pair of moves, or rather, resulting positions

* 1. <- move number
The move number is fairly simple, just the current move number

* e4 <- resulting white position
This is where the magic happens! This is the _resulting_ position
after white has moved, meaning that we reconstruct this from the state
and any piece that could possible land here.

* e5 <- resulting black position
The same thing as above, but for the black piece

    8 ♖♘♗♔♕♗♘♖          8 ♖♘♗♔♕♗♘♖          8 ♖♘♗♔♕♗♘♖
    7 ♙♙♙♙♙♙♙♙          7 ♙♙♙♙♙♙♙♙          7 ♙♙♙♙░♙♙♙
    6 ▒░▒░▒░▒░          6 ▒░▒░▒░▒░          6 ▒░▒░▒░▒░
    5 ░▒░▒░▒░▒          5 ░▒░▒░▒░▒          5 ░▒░▒♙▒░▒
1.  4 ▒░▒░▒░▒░ => e4 => 4 ▒░▒░♟░▒░ => e5 => 4 ▒░▒░♟░▒░
    3 ░▒░▒░▒░▒          3 ░▒░▒░▒░▒          3 ░▒░▒░▒░▒
    2 ♟♟♟♟♟♟♟♟          2 ♟♟♟♟▒♟♟♟          2 ♟♟♟♟▒♟♟♟
    1 ♜♞♝♛♚♝♞♜          1 ♜♞♝♛♚♝♞♜          1 ♜♞♝♛♚♝♞♜
      abcdefgh            abcdefgh            abcdefgh

Move two is basically the same, but note the capitalised N prefix

2. Nf3 Nc6

N means Knight, which is a designation used to more easily denote
which piece moved. No designation indicates a Pawn. A full list
is included below.

                          8 ♖░♗♔♕♗♘♖
The board after move two: 7 ♙♙♙♙░♙♙♙
                          6 ▒░♘░▒░▒░
                          5 ░▒░▒♙▒░▒
                          4 ▒░▒░♟░▒░
                          3 ░▒░▒░♞░▒
                          2 ♟♟♟♟▒♟♟♟
                          1 ♜♞♝♛♚♝░♜
                            abcdefgh

The next move is straightforward given what we know now:

3. Bb5 a6

8 ♖░♗♔♕♗♘♖
7 ░♙♙♙░♙♙♙
6 ♙░♘░▒░▒░
5 ░♝░▒♙▒░▒
4 ▒░▒░♟░▒░
3 ░▒░▒░♞░▒
2 ♟♟♟♟▒♟♟♟
1 ♜♞♝♛♚▒░♜
  abcdefgh

> newtype Notation = Notation [Row]
> data Row = Row {white :: Move, black :: Move}
> data Move = Move Piece (Maybe Pos) Pos

