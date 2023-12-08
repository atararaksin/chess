package chess

import chess.board.Board
import chess.piece.Piece

import scala.annotation.tailrec

object MoveHelpers {
  private val xChars = "abcdefgh".toCharArray

  def straightMoves(board: Board, piece: Piece): List[Int] = {
    var moves = consecutiveMoves(board, piece, Nil, 0, 1)
    moves = consecutiveMoves(board, piece, moves, 0, -1)
    moves = consecutiveMoves(board, piece, moves, 1, 0)
    moves = consecutiveMoves(board, piece, moves, -1, 0)
    moves
  }

  def diagonalMoves(board: Board, piece: Piece): List[Int] = {
    var moves = consecutiveMoves(board, piece, Nil, 1, 1)
    moves = consecutiveMoves(board, piece, moves, 1, -1)
    moves = consecutiveMoves(board, piece, moves, -1, 1)
    moves = consecutiveMoves(board, piece, moves, -1, -1)
    moves
  }

  def straightAndDiagonalMoves(board: Board, piece: Piece): List[Int] = {
    var moves = consecutiveMoves(board, piece, Nil, 0, 1)
    moves = consecutiveMoves(board, piece, moves, 0, -1)
    moves = consecutiveMoves(board, piece, moves, 1, 0)
    moves = consecutiveMoves(board, piece, moves, -1, 0)

    moves = consecutiveMoves(board, piece, moves, 1, 1)
    moves = consecutiveMoves(board, piece, moves, 1, -1)
    moves = consecutiveMoves(board, piece, moves, -1, 1)
    moves = consecutiveMoves(board, piece, moves, -1, -1)
    moves
  }

  private def consecutiveMoves(board: Board,
                               piece: Piece,
                               acc: List[Int],
                               xInc: Int, yInc: Int): List[Int] = {
    var square = piece.square
    var x = square % 8
    var y = square / 8

    var shouldStop = false

    var varAcc = acc

    do {
      x += xInc
      y += yInc
      if (x < 0 || y < 0 || x > 7 || y > 7) {
        shouldStop = true
      } else {
        square = y * 8 + x
        val other = board.getPiece(square)
        if (other.isDefined) {
          shouldStop = true
          if (other.get.isWhite != piece.isWhite) varAcc ::= square
        } else {
          varAcc ::= square
        }
      }
    } while (!shouldStop)

    varAcc
  }

  def isSquareUnderAttack(square: Int, availableOpponentMoves: List[(Piece, List[Int])]): Boolean = {
    availableOpponentMoves.exists(_._2.contains(square))
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
