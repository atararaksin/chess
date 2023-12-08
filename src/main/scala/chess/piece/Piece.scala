package chess.piece

import chess.board.Square

abstract class Piece {
  def id: Int
  def char: Char
  def x: Int
  def y: Int
  def value: Int
  def isWhite: Boolean
  def reprChar: Char =
    if (isWhite) char.toUpper else char

  def behavior: Behavior

  def dependentSquares: List[Square] =
    List()
  def moves: List[Square] =
    List()

}

object Piece extends Behavior {
  def fromChar(id: Int, char: Char, x: Int, y: Int): Piece = {
    val isWhite = char.isUpper
    val lowerChar = char.toLower

    if (lowerChar == 'p') Pawn(id, x, y, isWhite, List(), List())
    else if (lowerChar == 'r') Rook(id, x, y, isWhite, List(), List())
    else if (lowerChar == 'n') Knight(id, x, y, isWhite, List(), List())
    else if (lowerChar == 'b') Bishop(id, x, y, isWhite, List(), List())
    else if (lowerChar == 'q') Queen(id, x, y, isWhite, List(), List())
    else if (lowerChar == 'k') King(id, x, y, isWhite, List(), List())
    else sys.error(s"Unknown piece character: $char")
  }

  def fromOldPiece(oldPiece: Piece, toX: Int, toY: Int, newBoardSquares: Array[Option[Int]]): Piece = {
    val depSquares = oldPiece.behavior.calculateDependentSquares(toX, toY, oldPiece.isWhite, newBoardSquares)
    val moveSquares = oldPiece.behavior.calculateMoveSquares(toX, toY, oldPiece.isWhite, depSquares, newBoardSquares)

    if (oldPiece.isInstanceOf[Pawn]) Pawn(oldPiece.id, toX, toY, oldPiece.isWhite, depSquares, moveSquares)
    else if (oldPiece.isInstanceOf[Queen]) Queen(oldPiece.id, toX, toY, oldPiece.isWhite, depSquares, moveSquares)
    else if (oldPiece.isInstanceOf[Bishop]) Bishop(oldPiece.id, toX, toY, oldPiece.isWhite, depSquares, moveSquares)
    else if (oldPiece.isInstanceOf[Rook]) Rook(oldPiece.id, toX, toY, oldPiece.isWhite, depSquares, moveSquares)
    else if (oldPiece.isInstanceOf[Knight]) Knight(oldPiece.id, toX, toY, oldPiece.isWhite, depSquares, moveSquares)
    else if (oldPiece.isInstanceOf[King]) King(oldPiece.id, toX, toY, oldPiece.isWhite, depSquares, moveSquares)
    else sys.error(s"Unknown piece: $oldPiece")
  }
}
