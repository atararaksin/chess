package chess

import chess.board.Board
import chess.piece.King

object BestPosition {
  private val centerSquares = Set(3 * 8 + 3, 4 * 8 + 3, 3 * 8 + 4, 4 * 8 + 4)

  def positionScore(board: Board, asWhite: Boolean): Int = {
    val player = if (board.isWhitesTurn) board.white else board.black
    val oppositePlayer = if (board.isWhitesTurn) board.black else board.white

    val controlledSquares = player.pieces
      .filterNot(_.isInstanceOf[King])
      .flatMap(_.controlledSquares(board).takeWhile(s => s != -1))

    // 10 points for every controlled square
    // + more pints for every center square
    // + 3 points for every unique square
    // Depending on how many pieces are left in the game,
    // give from 0 to 4 points for a center square (more important in the beginning)
    val pointsForCenterSquare = ((((board.white.pieces.size + board.black.pieces.size).toDouble max 12) - 12) * 4 / 20).round.toInt
    val controlledSquaresScore = controlledSquares.length * 10 +
      controlledSquares.count(centerSquares(_)) * pointsForCenterSquare +
      controlledSquares.distinct.length * 3

    val attackScore = 4 * controlledSquares.flatMap(board.getPiece)
      .count(_.isWhite != asWhite)

    val underAttackScore = -4 * oppositePlayer.pieces
      .filterNot(_.isInstanceOf[King])
      .flatMap { piece =>
        piece.nextMoves(board).takeWhile(s => s != -1).flatMap(board.getPiece)
      }.count(_.isWhite == asWhite)

    controlledSquaresScore + attackScore + underAttackScore
  }
}
