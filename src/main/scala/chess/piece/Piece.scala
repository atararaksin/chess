package chess.piece

import chess.board.Board

abstract class Piece {
  val char: Char
  val square: Int
  val value: Int
  val isWhite: Boolean
  val maxMoves: Int // Max # of moves a piece of this type can theoretically have on a board
  def reprChar: Char =
    if (isWhite) char.toUpper else char

  def move(square: Int): Piece

  def nextMoves(board: Board): Array[Int] =
    Array.fill(1)(-1) // -1 as EOF token

  def controlledSquares(board: Board): Array[Int] =
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
