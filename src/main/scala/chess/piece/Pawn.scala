package chess.piece

import chess.board.Board

case class Pawn(override val square: Int,
                override val isWhite: Boolean) extends Piece {
  override val char = 'p'

  override val value = 1

  val maxMoves = 4

  override def move(toSquare: Int): Piece = {
    if ((isWhite && toSquare < 8) || (!isWhite && square > 55)) {
      // For only support promoting to Queen
      Queen(toSquare, isWhite)
    } else this.copy(square = toSquare)
  }

  override def nextMoves(board: Board): Array[Int] = {
    val fromX = square % 8
    val fromY = square / 8

    if (isWhite && fromY == 0 || !isWhite && fromY == 7) Array()
    else {
      val yInc = if (isWhite) -1 else 1
      val isStartPos = (isWhite && fromY == 6) || (!isWhite && fromY == 1)

      val moves = new Array[Int](maxMoves + 1)
      var i = 0
      val yForward = fromY + yInc
      var s = yForward * 8 + fromX
      if (board.getPiece(s).isEmpty) {
        moves(i) = s
        i += 1

        s = (yForward + yInc) * 8 + fromX
        if (isStartPos && board.getPiece(s).isEmpty) {
          moves(i) = s
          i += 1
        }
      }
      val xLeft = fromX - 1
      s = yForward * 8 + xLeft
      if (xLeft >= 0 && board.getPiece(s).exists(_.isWhite != isWhite)) {
        moves(i) = s
        i += 1
      }
      val xRight = fromX + 1
      s = yForward * 8 + xRight
      if (xRight <= 7 && board.getPiece(s).exists(_.isWhite != isWhite)) {
        moves(i) = s
        i += 1
      }

      moves(i) = -1
      moves
    }
  }

  override def controlledSquares(board: Board): Array[Int] = {
    val fromX = square % 8
    val fromY = square / 8

    if (isWhite && fromY == 0 || !isWhite && fromY == 7) Array()
    else {
      val moves = new Array[Int](maxMoves + 1)
      var i = 0
      val yInc = if (isWhite) -1 else 1
      val yForward = fromY + yInc
      val xLeft = fromX - 1
      var s = yForward * 8 + xLeft
      if (xLeft >= 0 && board.getPiece(s).exists(_.isWhite != isWhite)) {
        moves(i) = s
        i += 1
      }
      val xRight = fromX + 1
      s = yForward * 8 + xRight
      if (xRight <= 7 && board.getPiece(s).exists(_.isWhite != isWhite)) {
        moves(i) = s
        i += 1
      }

      moves(i) = -1
      moves
    }
  }

}