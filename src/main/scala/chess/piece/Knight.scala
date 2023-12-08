package chess.piece

import chess.board.Board

case class Knight(override val square: Int,
                  override val isWhite: Boolean) extends Piece {

  override val char = 'n'

  override val value = 3

  val maxMoves = 8

  override def move(square: Int): Piece =
    this.copy(square = square)

  override def nextMoves(board: Board): Array[Int] = {
    val moves = new Array[Int](maxMoves + 1)
    var i = 0

    val x = square % 8
    val y = square / 8

    var toY = y - 1
    var toX = x - 2
    var s = 0
    if (toY >= 0) {
      s = toY * 8 + toX
      if (toX >= 0 && !board.getPiece(s).exists(_.isWhite == isWhite)) {
        moves(i) = s
        i += 1
      }
      toX = x + 2
      s = toY * 8 + toX
      if (toX <= 7 && !board.getPiece(s).exists(_.isWhite == isWhite)) {
        moves(i) = s
        i += 1
      }
    }
    toY = y - 2
    toX = x - 1
    if (toY >= 0) {
      s = toY * 8 + toX
      if (toX >= 0 && !board.getPiece(s).exists(_.isWhite == isWhite)) {
        moves(i) = s
        i += 1
      }
      toX = x + 1
      s = toY * 8 + toX
      if (toX <= 7 && !board.getPiece(s).exists(_.isWhite == isWhite)) {
        moves(i) = s
        i += 1
      }
    }
    toY = y + 2
    toX = x - 1
    if (toY <= 7) {
      s = toY * 8 + toX
      if (toX >= 0 && !board.getPiece(s).exists(_.isWhite == isWhite)) {
        moves(i) = s
        i += 1
      }
      toX = x + 1
      s = toY * 8 + toX
      if (toX <= 7 && !board.getPiece(s).exists(_.isWhite == isWhite)) {
        moves(i) = s
        i += 1
      }
    }
    toY = y + 1
    toX = x - 2
    if (toY <= 7) {
      s = toY * 8 + toX
      if (toX >= 0 && !board.getPiece(s).exists(_.isWhite == isWhite)) {
        moves(i) = s
        i += 1
      }
      toX = x + 2
      s = toY * 8 + toX
      if (toX <= 7 && !board.getPiece(s).exists(_.isWhite == isWhite)) {
        moves(i) = s
        i += 1
      }
    }

    moves(i) = -1
    moves
  }
}
