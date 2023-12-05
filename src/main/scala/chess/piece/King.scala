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
    var moves: List[Square] = Nil

    var toY = y - 1
    var toX = x - 1
    if (toY >= 0) {
      if (toX >= 0 && !board.getPiece(toX, toY).exists(_.isWhite == isWhite)) {
        moves ::= Square(toX, toY)
      }
      toX = x + 1
      if (toX <= 7 && !board.getPiece(toX, toY).exists(_.isWhite == isWhite)) {
        moves ::= Square(toX, toY)
      }
      toX = x
      if (!board.getPiece(toX, toY).exists(_.isWhite == isWhite)) {
        moves ::= Square(toX, toY)
      }
    }

    toY = y + 1
    toX = x - 1
    if (toY <= 7) {
      if (toX >= 0 && !board.getPiece(toX, toY).exists(_.isWhite == isWhite)) {
        moves ::= Square(toX, toY)
      }
      toX = x + 1
      if (toX <= 7 && !board.getPiece(toX, toY).exists(_.isWhite == isWhite)) {
        moves ::= Square(toX, toY)
      }
      toX = x
      if (!board.getPiece(toX, toY).exists(_.isWhite == isWhite)) {
        moves ::= Square(toX, toY)
      }
    }

    toX = x - 1
    toY = y
    if (toX >= 0 && !board.getPiece(toX, toY).exists(_.isWhite == isWhite)) {
      moves ::= Square(toX, toY)
    }
    toX = x + 1
    if (toX <= 7 && !board.getPiece(toX, toY).exists(_.isWhite == isWhite)) {
      moves ::= Square(toX, toY)
    }

    moves
  }
}