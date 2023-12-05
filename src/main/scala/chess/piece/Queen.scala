package chess.piece

import chess.MoveHelpers
import chess.board.{Board, Square}

case class Queen(override val x: Int,
                 override val y: Int,
                 override val isWhite: Boolean) extends Piece {
  override val char = 'q'

  override val value = 9

  override def move(x: Int, y: Int): Piece =
    this.copy(x = x, y = y)

  override def nextMoves(board: Board): List[Square] =
    MoveHelpers.straightAndDiagonalMoves(board, this)
}
