package chess.piece

import chess.board.Square

case class King(override val id: Int,
                override val x: Int,
                override val y: Int,
                override val isWhite: Boolean,
                override val dependentSquares: List[Square],
                override val moves: List[Square]) extends Piece {
  override val char = 'k'

  override val value = 0

  override val behavior = King

}

object King extends Behavior {
  override def calculateDependentSquares(x: Int, y: Int, isWhite: Boolean, boardSquares: Array[Option[Int]]): List[Square] = {
    var squares: List[Square] = Nil

    var toY = y - 1
    var toX = x - 1
    if (toY >= 0) {
      if (toX >= 0) {
        squares ::= Square(toX, toY)
      }
      toX = x + 1
      if (toX <= 7) {
        squares ::= Square(toX, toY)
      }
      toX = x
      squares ::= Square(toX, toY)
    }

    toY = y + 1
    toX = x - 1
    if (toY <= 7) {
      if (toX >= 0) {
        squares ::= Square(toX, toY)
      }
      toX = x + 1
      if (toX <= 7) {
        squares ::= Square(toX, toY)
      }
      toX = x
      squares ::= Square(toX, toY)
    }

    toX = x - 1
    toY = y
    if (toX >= 0) {
      squares ::= Square(toX, toY)
    }
    toX = x + 1
    if (toX <= 7) {
      squares ::= Square(toX, toY)
    }

    squares
  }
}