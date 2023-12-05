package chess.piece

import chess.board.{Board, Square}

case class King(override val x: Int,
                override val y: Int,
                override val isWhite: Boolean) extends Piece {
  override val char = 'k'

  override val value = 0

  override def move(x: Int, y: Int): Piece =
    this.copy(x = x, y = y)

  override def nextMoves(board: Board): List[Square] = {
    for {
      (incX, incY) <- King.kingIncs
      toX = x + incX
      toY = y + incY
      if toX <= 7 && toX >= 0 && toY <= 7 && toY >= 0
      if !board.getPiece(toX, toY).exists(_.isWhite == isWhite)
    } yield Square(toX, toY)
  }
}

object King {
  val kingIncs: List[(Int, Int)] = for {
    incX <- List(0, 1, -1)
    incY <- List(0, 1, -1)
    if !(incX == 0 && incY == 0)
  } yield (incX, incY)
}