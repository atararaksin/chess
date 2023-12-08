package chess

import chess.board.Square
import chess.piece.Piece


object MoveHelpers {
  private val xChars = "abcdefgh".toCharArray

  def straightDependentSquares(boardSquares: Array[Option[Int]], fromX: Int, fromY: Int): List[Square] = {
    var moves = consecutiveDependentSquares(boardSquares, fromX, fromY, Nil, 0, 1)
    moves = consecutiveDependentSquares(boardSquares, fromX, fromY, moves, 0, -1)
    moves = consecutiveDependentSquares(boardSquares, fromX, fromY, moves, 1, 0)
    moves = consecutiveDependentSquares(boardSquares, fromX, fromY, moves, -1, 0)
    moves
  }

  def diagonalDependentSquares(boardSquares: Array[Option[Int]], fromX: Int, fromY: Int): List[Square] = {
    var moves = consecutiveDependentSquares(boardSquares, fromX, fromY, Nil, 1, 1)
    moves = consecutiveDependentSquares(boardSquares, fromX, fromY, moves, 1, -1)
    moves = consecutiveDependentSquares(boardSquares, fromX, fromY, moves, -1, 1)
    moves = consecutiveDependentSquares(boardSquares, fromX, fromY, moves, -1, -1)
    moves
  }

  def straightAndDiagonalDependentSquares(boardSquares: Array[Option[Int]], fromX: Int, fromY: Int): List[Square] = {
    var moves = consecutiveDependentSquares(boardSquares, fromX, fromY, Nil, 0, 1)
    moves = consecutiveDependentSquares(boardSquares, fromX, fromY, moves, 0, -1)
    moves = consecutiveDependentSquares(boardSquares, fromX, fromY, moves, 1, 0)
    moves = consecutiveDependentSquares(boardSquares, fromX, fromY, moves, -1, 0)

    moves = consecutiveDependentSquares(boardSquares, fromX, fromY, moves, 1, 1)
    moves = consecutiveDependentSquares(boardSquares, fromX, fromY, moves, 1, -1)
    moves = consecutiveDependentSquares(boardSquares, fromX, fromY, moves, -1, 1)
    moves = consecutiveDependentSquares(boardSquares, fromX, fromY, moves, -1, -1)
    moves
  }

  private def consecutiveDependentSquares(boardSquares: Array[Option[Int]],
                                          fromX: Int, fromY: Int,
                                          acc: List[Square],
                                          xInc: Int, yInc: Int): List[Square] = {
    var x = fromX
    var y = fromY

    var shouldStop = false

    var varAcc = acc

    do {
      x += xInc
      y += yInc
      if (x < 0 || y < 0 || x > 7 || y > 7) {
        shouldStop = true
      } else {
        varAcc ::= Square(x, y)
        if (boardSquares(y * 8 + x).isDefined) {
          shouldStop = true
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
