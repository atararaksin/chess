package chess

import chess.board.Board
import chess.piece.Piece

object MoveHelpers {
  private val xChars = "abcdefgh".toCharArray

  def straightMoves(board: Board, piece: Piece): Array[Int] = {
    val consecutiveMoves = new ConsecutiveMoves(board, piece)
    consecutiveMoves.addConsecutiveMoves(0, 1)
    consecutiveMoves.addConsecutiveMoves(0, -1)
    consecutiveMoves.addConsecutiveMoves(1, 0)
    consecutiveMoves.addConsecutiveMoves(-1, 0)
    consecutiveMoves.get()
  }

  def diagonalMoves(board: Board, piece: Piece): Array[Int] = {
    val consecutiveMoves = new ConsecutiveMoves(board, piece)
    consecutiveMoves.addConsecutiveMoves(1, 1)
    consecutiveMoves.addConsecutiveMoves(1, -1)
    consecutiveMoves.addConsecutiveMoves(-1, 1)
    consecutiveMoves.addConsecutiveMoves(-1, -1)
    consecutiveMoves.get()
  }

  def straightAndDiagonalMoves(board: Board, piece: Piece): Array[Int] = {
    val consecutiveMoves = new ConsecutiveMoves(board, piece)
    consecutiveMoves.addConsecutiveMoves(0, 1)
    consecutiveMoves.addConsecutiveMoves(0, -1)
    consecutiveMoves.addConsecutiveMoves(1, 0)
    consecutiveMoves.addConsecutiveMoves(-1, 0)

    consecutiveMoves.addConsecutiveMoves(1, 1)
    consecutiveMoves.addConsecutiveMoves(1, -1)
    consecutiveMoves.addConsecutiveMoves(-1, 1)
    consecutiveMoves.addConsecutiveMoves(-1, -1)
    consecutiveMoves.get()
  }

  def isSquareUnderAttack(square: Int, availableOpponentMoves: List[(Piece, Array[Int])]): Boolean = {
    availableOpponentMoves.exists { pieceAndMoves =>
      val moves = pieceAndMoves._2
      var found = false
      var i = 0
      var s = moves(i)
      while (!(found || s == -1)) {
        if (s == square) found = true
        i += 1
        s = moves(i)
      }
      found
    }
  }

  def encodeMove(piece: Piece, targetSquare: Int): String = {
    def encodeSquare(square: Int) = {
      val x = square % 8
      val y = square / 8
      xChars(x) + (8 - y).toString
    }
    s"${piece.reprChar}-${encodeSquare(piece.square)}${encodeSquare(targetSquare)}"
  }
}
