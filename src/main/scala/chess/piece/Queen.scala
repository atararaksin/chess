package chess.piece

import chess.MoveHelpers
import chess.board.Board

case class Queen(override val square: Int,
                 override val isWhite: Boolean) extends Piece {
  override val char = 'q'

  override val value = 9

  override def move(square: Int): Piece =
    this.copy(square = square)

  override def nextMoves(board: Board): List[Int] =
    MoveHelpers.straightAndDiagonalMoves(board, this)
}
