{-# LANGUAGE TupleSections #-}
module Game.Chessy.Board where

import Debug.Trace

import Data.Bifunctor (second)
import Data.Ix (inRange)
import Data.Array (Array, listArray, assocs, (!), bounds)
import Data.Char (ord, toUpper)
import Data.List.Split (chunksOf)

class PrettyPrint a where
  pp :: a -> String

newtype Position = Pos { unpos :: (Int, Char) }

newtype Board = Board {unBoard :: Array (Int, Char) (Maybe Cell)}
  deriving (Show, Eq)

instance PrettyPrint Board where
  pp = (++ header) . concatMap decorateRow . reverse . chunksOf 8 . map ppCell . assocs . unBoard
    where ppCell :: ((Int, Char), Maybe Cell) -> ((Int, Char), String)
          ppCell (p, Just c) = (p, pp c)
          ppCell (p@(ii, ic), Nothing)
            | odd (ord ic + ii) = (p, "▒")
            | otherwise         = (p, "░")
          decorateRow :: [((Int, Char), String)] -> String
          decorateRow xs = let row = fst . fst $ head xs
                           in show row ++ " " ++ concatMap snd xs ++ "\n"
          header = replicate 2 ' ' ++ ['a'..'h'] ++ "\n"

newtype Cell = Cell {unCell :: (,) Color Piece}
  deriving (Show, Eq)

data Color = White | Black
  deriving (Show, Eq)

data Piece = Pawn | Knight | Bishop | Rook | King | Queen
  deriving (Show, Eq)

instance PrettyPrint Cell where
  pp (Cell (c, p)) = case c of
    Black -> case p of
      King   -> "♔"
      Queen  -> "♕"
      Rook   -> "♖"
      Bishop -> "♗"
      Knight -> "♘"
      Pawn   -> "♙"
    White -> case p of
      King   -> "♚"
      Queen  -> "♛"
      Rook   -> "♜"
      Bishop -> "♝"
      Knight -> "♞"
      Pawn   -> "♟"


blackLineup :: [Cell]
blackLineup = map (Cell . (Black, )) [Rook, Knight, Bishop, King, Queen, Bishop, Knight, Rook]

whiteLineup :: [Cell]
whiteLineup = map (Cell . (White, )) [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]

rowOf :: Color -> Piece -> [Cell]
rowOf c p = replicate 8 $ Cell (c, p)

(@@) :: Board -> Position -> Maybe Cell
(Board b) @@ (Pos p)
  | inRange (bounds b) p' = b ! p'
  | otherwise             = Nothing
  where p' = second toUpper p

newChessBoard :: Board
newChessBoard = Board . listArray ((1, 'A'),(8, 'H')) . concat
                $ map (map Just) [ whiteLineup, rowOf White Pawn ]
                ++ replicate 4 (replicate 8 Nothing)
                ++ map (map Just) [ rowOf Black Pawn, blackLineup ]

