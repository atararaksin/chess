package chess.piece

import chess.board.{Board, Square}

case class Knight(override val x: Int,
                  override val y: Int,
                  override val isWhite: Boolean) extends Piece {

  override val char = 'n'

  override val value = 3

  override def move(x: Int, y: Int): Piece =
    this.copy(x = x, y = y)

  override def nextMoves(board: Board): List[Square] = {
    for {
      (incX, incY) <- Knight.knightIncs
      toX = x + incX
      toY = y + incY
      if toX <= 7 && toX >= 0 && toY <= 7 && toY >= 0
      if !board.getPiece(toX, toY).exists(_.isWhite == isWhite)
    } yield Square(toX, toY)
  }
}

object Knight {
  val knightIncs: List[(Int, Int)] = for {
    (incX, incY) <- List((1, 2), (2, 1))
    multX <- List(1, -1)
    multY <- List(1, -1)
  } yield (incX * multX, incY * multY)
}
