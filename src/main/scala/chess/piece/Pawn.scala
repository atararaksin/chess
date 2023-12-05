package chess.piece

import chess.board.{Board, Square}

case class Pawn(override val x: Int,
                override val y: Int,
                override val isWhite: Boolean) extends Piece {
  override val char = 'p'

  override val value = 1

  override def move(x: Int, y: Int): Piece =
    this.copy(x = x, y = y)

  override def nextMoves(board: Board): List[Square] = {
    val fromX = x
    val fromY = y

    val yInc = if (isWhite) -1 else 1

    val isStartPos = (isWhite && fromY == 6) || (!isWhite && fromY == 1)

    def isVacantSquare(x: Int, y: Int) =
      x <= 7 && x >= 0 && y <= 7 && y >= 0 && board.getPiece(x, y).isEmpty

    def isEnemySquare(x: Int, y: Int) =
      x <= 7 && x >= 0 && y <= 7 && y >= 0 && board.getPiece(x, y).exists(_.isWhite != isWhite)

    List(
      if (isVacantSquare(fromX, fromY + yInc)) Some(Square(fromX, fromY + yInc))
      else None,
      if (isStartPos && isVacantSquare(fromX, fromY + yInc) && isVacantSquare(fromX, fromY + 2 * yInc)) Some(Square(fromX, fromY + 2 * yInc))
      else None,
      if (isEnemySquare(fromX - 1, fromY + yInc)) Some(Square(fromX - 1, fromY + yInc))
      else None,
      if (isEnemySquare(fromX + 1, fromY + yInc)) Some(Square(fromX + 1, fromY + yInc))
      else None
    ).flatten
  }

}