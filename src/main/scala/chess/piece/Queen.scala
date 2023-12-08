package chess.piece

import chess.MoveHelpers
import chess.board.Square

case class Queen(override val id: Int,
                 override val x: Int,
                 override val y: Int,
                 override val isWhite: Boolean,
                 override val dependentSquares: List[Square],
                 override val moves: List[Square]) extends Piece {
  override val char = 'q'

  override val value = 9

  override val behavior = Queen
}

object Queen extends Behavior {
  override def calculateDependentSquares(x: Int, y: Int, isWhite: Boolean, boardSquares: Array[Option[Int]]): List[Square] =
    MoveHelpers.straightAndDiagonalDependentSquares(boardSquares, x, y)
}
