package chess.piece

import chess.board.Board

abstract class Piece {
  def char: Char
  def square: Int
  def value: Int
  def isWhite: Boolean
  def reprChar: Char =
    if (isWhite) char.toUpper else char

  def move(square: Int): Piece

  def nextMoves(board: Board): List[Int] =
    List()

  def controlledSquares(board: Board): List[Int] =
    nextMoves(board)
}

object Piece {
  def fromChar(char: Char, square: Int): Piece = {
    val isWhite = char.isUpper
    val lowerChar = char.toLower

    if (lowerChar == 'p') Pawn(square, isWhite)
    else if (lowerChar == 'r') Rook(square, isWhite)
    else if (lowerChar == 'n') Knight(square, isWhite)
    else if (lowerChar == 'b') Bishop(square, isWhite)
    else if (lowerChar == 'q') Queen(square, isWhite)
    else if (lowerChar == 'k') King(square, isWhite)
    else sys.error(s"Unknown piece character: $char")
  }
}
