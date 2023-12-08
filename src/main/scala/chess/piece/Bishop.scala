package chess.piece

import chess.MoveHelpers
import chess.board.Board

case class Bishop(override val square: Int,
                  override val isWhite: Boolean) extends Piece {
  override val char = 'b'

  override val value = 3

  val maxMoves = 14

  override def move(square: Int): Piece =
    this.copy(square = square)

  override def nextMoves(board: Board): Array[Int] =
    MoveHelpers.diagonalMoves(board, this)
}
