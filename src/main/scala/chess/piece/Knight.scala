package chess.piece

import chess.board.Board

case class Knight(override val square: Int,
                  override val isWhite: Boolean) extends Piece {

  override val char = 'n'

  override val value = 3

  override def move(square: Int): Piece =
    this.copy(square = square)

  override def nextMoves(board: Board): List[Int] = {
    var moves: List[Int] = Nil

    val x = square % 8
    val y = square / 8

    var toY = y - 1
    var toX = x - 2
    var s = 0
    if (toY >= 0) {
      s = toY * 8 + toX
      if (toX >= 0 && !board.getPiece(s).exists(_.isWhite == isWhite)) moves ::= s
      toX = x + 2
      s = toY * 8 + toX
      if (toX <= 7 && !board.getPiece(s).exists(_.isWhite == isWhite)) moves ::= s
    }
    toY = y - 2
    toX = x - 1
    if (toY >= 0) {
      s = toY * 8 + toX
      if (toX >= 0 && !board.getPiece(s).exists(_.isWhite == isWhite)) moves ::= s
      toX = x + 1
      s = toY * 8 + toX
      if (toX <= 7 && !board.getPiece(s).exists(_.isWhite == isWhite)) moves ::= s
    }
    toY = y + 2
    toX = x - 1
    if (toY <= 7) {
      s = toY * 8 + toX
      if (toX >= 0 && !board.getPiece(s).exists(_.isWhite == isWhite)) moves ::= s
      toX = x + 1
      s = toY * 8 + toX
      if (toX <= 7 && !board.getPiece(s).exists(_.isWhite == isWhite)) moves ::= s
    }
    toY = y + 1
    toX = x - 2
    if (toY <= 7) {
      s = toY * 8 + toX
      if (toX >= 0 && !board.getPiece(s).exists(_.isWhite == isWhite)) moves ::= s
      toX = x + 2
      s = toY * 8 + toX
      if (toX <= 7 && !board.getPiece(s).exists(_.isWhite == isWhite)) moves ::= s
    }

    moves
  }
}
