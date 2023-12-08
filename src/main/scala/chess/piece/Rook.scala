package chess.piece

import chess.MoveHelpers
import chess.board.Board

case class Rook(override val square: Int,
                override val isWhite: Boolean) extends Piece {
  override val char = 'r'

  override val value = 5

  override def move(square: Int): Piece =
    this.copy(square = square)

  override def nextMoves(board: Board): List[Int] =
    MoveHelpers.straightMoves(board, this)

}