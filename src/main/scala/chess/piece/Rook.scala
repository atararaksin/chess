package chess.piece

import chess.MoveHelpers
import chess.board.{Board, Square}

case class Rook(override val x: Int,
                override val y: Int,
                override val isWhite: Boolean) extends Piece {
  override val char = 'r'

  override val value = 5

  override def move(x: Int, y: Int): Piece =
    this.copy(x = x, y = y)

  override def nextMoves(board: Board): List[Square] =
    MoveHelpers.straightMoves(board, this)

}