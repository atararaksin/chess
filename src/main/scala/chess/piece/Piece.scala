package chess.piece

import chess.board.{Board, Square}

abstract class Piece {
  def char: Char
  def x: Int
  def y: Int
  def value: Int
  def isWhite: Boolean
  def reprChar: Char =
    if (isWhite) char.toUpper else char

  def move(x: Int, y: Int): Piece

  def nextMoves(board: Board): List[Square] =
    List()

  def controlledSquares(board: Board): List[Square] =
    nextMoves(board)
}

object Piece {
  def fromChar(char: Char, x: Int, y: Int): Piece = {
    val isWhite = char.isUpper
    val lowerChar = char.toLower

    if (lowerChar == 'p') Pawn(x, y, isWhite)
    else if (lowerChar == 'r') Rook(x, y, isWhite)
    else if (lowerChar == 'n') Knight(x, y, isWhite)
    else if (lowerChar == 'b') Bishop(x, y, isWhite)
    else if (lowerChar == 'q') Queen(x, y, isWhite)
    else if (lowerChar == 'k') King(x, y, isWhite)
    else sys.error(s"Unknown piece character: $char")
  }
}
