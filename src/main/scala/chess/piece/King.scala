package chess.piece

import chess.board.Board

case class King(override val square: Int,
                override val isWhite: Boolean) extends Piece {
  override val char = 'k'

  override val value = 0

  override def move(square: Int): Piece =
    this.copy(square = square)

  override def nextMoves(board: Board): List[Int] = {
    val x = square % 8
    val y = square / 8

    var moves: List[Int] = Nil

    var toY = y - 1
    var toX = x - 1
    var s = 0
    if (toY >= 0) {
      s = toY * 8 + toX
      if (toX >= 0 && !board.getPiece(s).exists(_.isWhite == isWhite)) {
        moves ::= s
      }
      toX = x + 1
      s = toY * 8 + toX
      if (toX <= 7 && !board.getPiece(s).exists(_.isWhite == isWhite)) {
        moves ::= s
      }
      toX = x
      s = toY * 8 + toX
      if (!board.getPiece(s).exists(_.isWhite == isWhite)) {
        moves ::= s
      }
    }

    toY = y + 1
    toX = x - 1
    if (toY <= 7) {
      s = toY * 8 + toX
      if (toX >= 0 && !board.getPiece(s).exists(_.isWhite == isWhite)) {
        moves ::= s
      }
      toX = x + 1
      s = toY * 8 + toX
      if (toX <= 7 && !board.getPiece(s).exists(_.isWhite == isWhite)) {
        moves ::= s
      }
      toX = x
      s = toY * 8 + toX
      if (!board.getPiece(s).exists(_.isWhite == isWhite)) {
        moves ::= s
      }
    }

    toX = x - 1
    toY = y
    s = toY * 8 + toX
    if (toX >= 0 && !board.getPiece(s).exists(_.isWhite == isWhite)) {
      moves ::= s
    }
    toX = x + 1
    s = toY * 8 + toX
    if (toX <= 7 && !board.getPiece(s).exists(_.isWhite == isWhite)) {
      moves ::= s
    }

    moves
  }
}