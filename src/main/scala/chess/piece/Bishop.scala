package chess.piece

import chess.MoveHelpers
import chess.board.Square

case class Bishop(override val id: Int,
                  override val x: Int,
                  override val y: Int,
                  override val isWhite: Boolean,
                  override val dependentSquares: List[Square],
                  override val moves: List[Square]) extends Piece {
  override val char = 'b'

  override val value = 3

  override val behavior = Bishop
}

object Bishop extends Behavior {
  override def calculateDependentSquares(x: Int, y: Int, isWhite: Boolean, boardSquares: Array[Option[Int]]): List[Square] =
    MoveHelpers.diagonalDependentSquares(boardSquares, x, y)
}
