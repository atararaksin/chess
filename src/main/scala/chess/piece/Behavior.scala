package chess.piece

import chess.board.{Board, Square}

trait Behavior {
  def calculateDependentSquares(x: Int, y: Int, isWhite: Boolean,
                                boardSquares: Array[Option[Int]]): List[Square] =
    List()

  def calculateMoveSquares(x: Int, y: Int, isWhite: Boolean,
                           dependentSquares: List[Square], boardSquares: Array[Option[Int]]): List[Square] = {
    var squares: List[Square] = Nil
    for (depSquare <- dependentSquares) {
      val boardSquare = boardSquares(depSquare.y * 8 + depSquare.x)
      if (boardSquare == None) squares ::= depSquare
      else if (isWhite ^ boardSquare.get < 16) squares ::= depSquare
    }
    squares
  }

  def calculateControlledSquares(piece: Piece, board: Board): List[Square] =
    piece.moves
}
