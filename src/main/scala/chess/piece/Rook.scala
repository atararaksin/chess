package chess.piece

import chess.MoveHelpers
import chess.board.Square

case class Rook(override val id: Int,
                override val x: Int,
                override val y: Int,
                override val isWhite: Boolean,
                override val dependentSquares: List[Square],
                override val moves: List[Square]) extends Piece {
  override val char = 'r'

  override val value = 5

  override val behavior = Rook
}

object Rook extends Behavior {
  override def calculateDependentSquares(x: Int, y: Int, isWhite: Boolean, boardSquares: Array[Option[Int]]): List[Square] =
    MoveHelpers.straightDependentSquares(boardSquares, x, y)
}