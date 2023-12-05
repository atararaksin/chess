package chess

import chess.board.{Board, Square}
import chess.piece.Piece

import scala.annotation.tailrec

object MoveHelpers {
  private val straightIncs = List((0, 1), (0, -1), (1, 0), (-1, 0))
  private val diagonalIncs = List((1, 1), (1, -1), (-1, 1), (-1, -1))
  private val xChars = "abcdefgh".toCharArray

  def straightMoves(board: Board, piece: Piece): List[Square] =
    consecutiveMoves(board, piece, straightIncs)

  def diagonalMoves(board: Board, piece: Piece): List[Square] =
    consecutiveMoves(board, piece, diagonalIncs)

  private def consecutiveMoves(board: Board,
                               piece: Piece,
                               incs: List[(Int, Int)]): List[Square] = {
    @tailrec
    def loop(acc: List[Square], x: Int, y: Int, xInc: Int, yInc: Int): List[Square] = {
      val toX = x + xInc
      val toY = y + yInc

      if (toX > 7 || toX < 0 || toY > 7 || toY < 0) acc
      else board.getPiece(toX, toY) match {
        case None => loop(Square(toX, toY)::acc, toX, toY, xInc, yInc)
        case Some(other) if other.isWhite == piece.isWhite => acc
        case _ => Square(toX, toY)::acc
      }
    }

    for {
      (xInc, yInc) <- incs
      square <- loop(Nil, piece.x, piece.y, xInc, yInc)
    } yield square
  }

  def isSquareUnderAttack(x: Int, y: Int, availableOpponentMoves: List[(Piece, Square)]): Boolean = {
    availableOpponentMoves.exists(s => s._2.x == x && s._2.y == y)
  }

  def encodeMove(piece: Piece, targetSquare: Square): String = {
    def encodeSquare(x: Int, y: Int) = xChars(x) + (8 - y).toString
    s"${piece.reprChar}-${encodeSquare(piece.x, piece.y)}${encodeSquare(targetSquare.x, targetSquare.y)}"
  }
}
