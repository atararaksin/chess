package chess

import chess.board.{Board, Square}
import chess.piece.Piece

import scala.annotation.tailrec

object MoveHelpers {
  private val xChars = "abcdefgh".toCharArray

  def straightMoves(board: Board, piece: Piece): List[Square] = {
    var moves = consecutiveMoves(board, piece, Nil, 0, 1)
    moves = consecutiveMoves(board, piece, moves, 0, -1)
    moves = consecutiveMoves(board, piece, moves, 1, 0)
    moves = consecutiveMoves(board, piece, moves, -1, 0)
    moves
  }

  def diagonalMoves(board: Board, piece: Piece): List[Square] = {
    var moves = consecutiveMoves(board, piece, Nil, 1, 1)
    moves = consecutiveMoves(board, piece, moves, 1, -1)
    moves = consecutiveMoves(board, piece, moves, -1, 1)
    moves = consecutiveMoves(board, piece, moves, -1, -1)
    moves
  }

  def straightAndDiagonalMoves(board: Board, piece: Piece): List[Square] = {
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
                               acc: List[Square],
                               xInc: Int, yInc: Int): List[Square] = {
    var x = piece.x
    var y = piece.y

    var shouldStop = false

    var varAcc = acc

    do {
      x += xInc
      y += yInc
      if (x < 0 || y < 0 || x > 7 || y > 7) {
        shouldStop = true
      } else {
        val other = board.getPiece(x, y)
        if (other.isDefined) {
          shouldStop = true
          if (other.get.isWhite != piece.isWhite) varAcc ::= Square(x, y)
        } else {
          varAcc ::= Square(x, y)
        }
      }
    } while (!shouldStop)

    varAcc
  }

  def isSquareUnderAttack(x: Int, y: Int, availableOpponentMoves: List[(Piece, Square)]): Boolean = {
    availableOpponentMoves.exists(s => s._2.x == x && s._2.y == y)
  }

  def encodeMove(piece: Piece, targetSquare: Square): String = {
    def encodeSquare(x: Int, y: Int) = xChars(x) + (8 - y).toString
    s"${piece.reprChar}-${encodeSquare(piece.x, piece.y)}${encodeSquare(targetSquare.x, targetSquare.y)}"
  }
}
