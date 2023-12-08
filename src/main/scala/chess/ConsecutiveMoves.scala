package chess

import chess.board.Board
import chess.piece.Piece

class ConsecutiveMoves(board: Board, piece: Piece) {

  private val square = piece.square
  private val fromX = square % 8
  private val fromY = square / 8
  private val isWhite = piece.isWhite
  private val maxMoves = piece.maxMoves

  private val moves = new Array[Int](maxMoves + 1)
  private var i = 0

  def addConsecutiveMoves(xInc: Int, yInc: Int): Unit = {
    var x = fromX
    var y = fromY

    var s = 0

    var shouldStop = false

    do {
      x += xInc
      y += yInc
      if (x < 0 || y < 0 || x > 7 || y > 7) {
        shouldStop = true
      } else {
        s = y * 8 + x
        val other = board.getPiece(s)
        if (other.isDefined) {
          shouldStop = true
          if (other.get.isWhite != isWhite) {
            moves(i) = s
            i += 1
          }
        } else {
          moves(i) = s
          i += 1
        }
      }
    } while (!shouldStop)
  }

  def get(): Array[Int] = {
    moves(i) = -1
    moves
  }
}
