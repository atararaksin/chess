package chess.piece

import chess.board.{Board, Square}

case class Pawn(override val id: Int,
                override val x: Int,
                override val y: Int,
                override val isWhite: Boolean,
                override val dependentSquares: List[Square],
                override val moves: List[Square]) extends Piece {
  override val char = 'p'

  override val value = 1

  override val behavior = Pawn
}

object Pawn extends Behavior {
  override def calculateDependentSquares(x: Int, y: Int, isWhite: Boolean, boardSquares: Array[Option[Int]]): List[Square] = {
    val fromX = x
    val fromY = y

    if (isWhite && fromY == 0 || !isWhite && fromY == 7) Nil
    else {
      val yInc = if (isWhite) -1 else 1
      val isStartPos = (isWhite && fromY == 6) || (!isWhite && fromY == 1)

      var squares: List[Square] = Nil
      val yForward = fromY + yInc
      squares ::= Square(fromX, yForward)
      if (isStartPos && boardSquares(yForward * 8 + fromX).isEmpty) {
        squares ::= Square(fromX, yForward + yInc)
      }
      val xLeft = fromX - 1
      if (xLeft >= 0) {
        squares ::= Square(xLeft, yForward)
      }
      val xRight = fromX + 1
      if (xRight <= 7) {
        squares ::= Square(xRight, yForward)
      }

      squares
    }
  }

  override def calculateMoveSquares(x: Int, y: Int, isWhite: Boolean,
                                    dependentSquares: List[Square],
                                    boardSquares: Array[Option[Int]]): List[Square] = {
    dependentSquares.filter { s =>
      (s.x == x && boardSquares(s.y * 8 + s.x).isEmpty) ||
        boardSquares(s.y * 8 + s.x).exists(isWhite ^ _ < 16)
    }
  }

  override def calculateControlledSquares(piece: Piece, board: Board): List[Square] = {
    val fromX = piece.x
    val fromY = piece.y

    if (piece.isWhite && fromY == 0 || !piece.isWhite && fromY == 7) Nil
    else {
      var squares: List[Square] = Nil
      val yInc = if (piece.isWhite) -1 else 1
      val yForward = fromY + yInc
      val xLeft = fromX - 1
      if (xLeft >= 0 && board.getPiece(xLeft, yForward).exists(_.isWhite != piece.isWhite)) {
        squares ::= Square(xLeft, yForward)
      }
      val xRight = fromX + 1
      if (xRight <= 7 && board.getPiece(xRight, yForward).exists(_.isWhite != piece.isWhite)) {
        squares ::= Square(xRight, yForward)
      }

      squares
    }
  }
}