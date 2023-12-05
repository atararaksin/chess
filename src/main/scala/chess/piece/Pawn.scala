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

    if (isWhite && fromY == 0 || !isWhite && fromY == 7) Nil
    else {
      val yInc = if (isWhite) -1 else 1
      val isStartPos = (isWhite && fromY == 6) || (!isWhite && fromY == 1)

      var moves: List[Square] = Nil
      val yForward = fromY + yInc
      if (board.getPiece(fromX, yForward).isEmpty) {
        moves ::= Square(fromX, yForward)
        if (isStartPos && board.getPiece(fromX, yForward + yInc).isEmpty) {
          moves ::= Square(fromX, yForward + yInc)
        }
      }
      val xLeft = fromX - 1
      if (xLeft >= 0 && board.getPiece(xLeft, yForward).exists(_.isWhite != isWhite)) {
        moves ::= Square(xLeft, yForward)
      }
      val xRight = fromX + 1
      if (xRight <= 7 && board.getPiece(xRight, yForward).exists(_.isWhite != isWhite)) {
        moves ::= Square(xRight, yForward)
      }

      moves
    }
  }

}