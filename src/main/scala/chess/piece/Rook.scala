package chess.piece

import chess.MoveHelpers
import chess.board.Board

case class Rook(override val square: Int,
                override val isWhite: Boolean) extends Piece {
  override val char = 'r'

  override val value = 5

  val maxMoves = 14

  override def move(square: Int): Piece =
    this.copy(square = square)

  override def nextMoves(board: Board): Array[Int] =
    MoveHelpers.straightMoves(board, this)

}