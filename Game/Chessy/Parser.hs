module Chessy.Game.Parser where

import Chessy.Game.Board

data Position = Position Char Int

data AlgebraicChessNotationEntry
  = KingsideCastling
  | QueensideCastling
  | EndOfGame (Maybe Color)
  | Move Piece Position Position
  | Check AlgebraicChessNotationEntry
  | DoubleCheck AlgebraicChessNotationEntry
  | Promotion 
