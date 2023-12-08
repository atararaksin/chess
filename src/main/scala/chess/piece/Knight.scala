package chess.piece

import chess.board.Square

case class Knight(override val id: Int,
                  override val x: Int,
                  override val y: Int,
                  override val isWhite: Boolean,
                  override val dependentSquares: List[Square],
                  override val moves: List[Square]) extends Piece {

  override val char = 'n'

  override val value = 3

  override val behavior = Knight
}

object Knight extends Behavior {
  override def calculateDependentSquares(x: Int, y: Int, isWhite: Boolean, boardSquares: Array[Option[Int]]): List[Square] = {
    var squares: List[Square] = Nil

    var toY = y - 1
    var toX = x - 2
    if (toY >= 0) {
      if (toX >= 0) squares ::= Square(toX, toY)
      toX = x + 2
      if (toX <= 7) squares ::= Square(toX, toY)
    }
    toY = y - 2
    toX = x - 1
    if (toY >= 0) {
      if (toX >= 0) squares ::= Square(toX, toY)
      toX = x + 1
      if (toX <= 7) squares ::= Square(toX, toY)
    }
    toY = y + 2
    toX = x - 1
    if (toY <= 7) {
      if (toX >= 0) squares ::= Square(toX, toY)
      toX = x + 1
      if (toX <= 7) squares ::= Square(toX, toY)
    }
    toY = y + 1
    toX = x - 2
    if (toY <= 7) {
      if (toX >= 0) squares ::= Square(toX, toY)
      toX = x + 2
      if (toX <= 7) squares ::= Square(toX, toY)
    }

    squares
  }
}