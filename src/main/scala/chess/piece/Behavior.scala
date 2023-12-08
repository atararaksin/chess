package chess.piece

import chess.board.{Board, Square}

trait Behavior {
  def calculateDependentSquares(x: Int, y: Int, isWhite: Boolean,
                                boardSquares: Array[Option[Int]]): List[Square] =
    List()

  def calculateMoveSquares(x: Int, y: Int, isWhite: Boolean,
                           dependentSquares: List[Square], boardSquares: Array[Option[Int]]): List[Square] =
    dependentSquares.filter { s =>
      !boardSquares(s.y * 8 + s.x).exists(pId => isWhite ^ pId >= 16)
    }

  def calculateControlledSquares(piece: Piece, board: Board): List[Square] =
    piece.moves
}
