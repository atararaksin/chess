package chess

import chess.board.Board
import chess.piece.Piece

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong
import scala.collection.GenSeq
import scala.util.Random

case class Game(score: Int, scoreAchievedAt: Int, moves: List[String])
case class CachedGame(cachedAtDepth: Int, game: Game)

class BestMove {
  private val parallelUntilDepth = 3 //3

  private val visited: ConcurrentHashMap[String, CachedGame] = new ConcurrentHashMap()

  val totalGamesExplored = new AtomicLong()
  val totalMovesMade = new AtomicLong()
  val shortcircuitedGames = new AtomicLong()
  var elapsedMillis = 0L

  def findBestMove(board: Board, targetDepth: Int, depthHardLimit: Int): Game = {
    totalGamesExplored.set(0)
    totalMovesMade.set(0)
    shortcircuitedGames.set(0)

    visited.clear()

    val startTime = System.currentTimeMillis()

    val game = bestGame(board, 0, board.isWhitesTurn, targetDepth, depthHardLimit)

    elapsedMillis = System.currentTimeMillis() - startTime

    game
  }

  private def score(board: Board, asWhite: Boolean): Int =
    if (asWhite) board.white.score - board.black.score
    else board.black.score - board.white.score

  private def gameEnd(board: Board, depth: Int, asWhite: Boolean): Game = {
    totalGamesExplored.incrementAndGet()
    Game(score(board, asWhite), depth, Nil)
  }

  private def mate(board: Board, depth: Int, asWhite: Boolean): Game = {
    totalGamesExplored.incrementAndGet()
    val score = if (board.isWhitesTurn == asWhite) -999 else 999
    Game(score, depth, Nil)
  }

  private def staleMate(depth: Int): Game = {
    totalGamesExplored.incrementAndGet()
    Game(0, depth, Nil)
  }

  private def cachedGame(boardEncoding: String, depth: Int): Option[CachedGame] =
    Option(visited.get(boardEncoding)).filter(_.cachedAtDepth <= depth)

  private def bestGameByMaterialAndPosition(games: GenSeq[(Board, Game)], asWhite: Boolean): Game = {
    // 1st priority - material, 2nd - how fast material score was achieved, 3rd - position score
    def gameScore(game: Game) = game.score * 1000 - game.scoreAchievedAt
    val maxMaterialScore = games.map(g => gameScore(g._2)).max
    val gamesWithBestGameScore = games.filter(g => gameScore(g._2) == maxMaterialScore)
    val gamesAndPositionScores = gamesWithBestGameScore.map { case (board, game) =>
      (game, BestPosition.positionScore(board, asWhite))
    }
    val bestScore = gamesAndPositionScores.map(_._2).max
    val worstScore = gamesAndPositionScores.map(_._2).min
    println(s"Got ${gamesWithBestGameScore.size} games with the same material score and depth to score.")
    println(s"Choosing the best game by position score, ranging from $worstScore to $bestScore.")
    Random.shuffle(gamesAndPositionScores.filter(_._2 == bestScore).toList).head._1
  }

  private def bestGameByMaterialAndDepth(games: GenSeq[(Board, Game)], asWhite: Boolean, isWhitesTurn: Boolean): Game = {
    if (asWhite == isWhitesTurn) games.maxBy(g => g._2.score * 1000 - g._2.scoreAchievedAt)._2
    else games.minBy(g => g._2.score * 1000 - g._2.scoreAchievedAt)._2
  }

  private def getNextMoveSummaries(board: Board): List[(Piece, Int, Board, Boolean)] = {
    var moves: List[(Piece, Int, Board, Boolean)] = Nil
    for (pieceAndMoves <- board.nextMoves) {
      val piece = pieceAndMoves._1
      val pieceMoves = pieceAndMoves._2

      var i = 0
      var square = pieceMoves(i)
      while (square != -1) {
        val newBoard = board.movePiece(piece, square)
        if (!newBoard.isPreviousPlayerInCheck) {
          moves ::= (piece, square, newBoard, board.getPiece(square).isDefined)
        }
        i += 1
        square = pieceMoves(i)
      }
    }
    moves
  }

  def bestGame(board: Board, depth: Int, asWhite: Boolean, targetDepth: Int, depthHardLimit: Int): Game = {
    cachedGame(board.encoding, depth) match {
      case Some(cachedGame) =>
        shortcircuitedGames.incrementAndGet()
        cachedGame.game
      case None => {
        val game = if (board.nextMoves.isEmpty) { // End of game, could be either mate or stalemate
          if (board.isCurrentPlayerInCheck) mate(board, depth, asWhite)
          else staleMate(depth)
        } else if (depth >= depthHardLimit) {
          gameEnd(board, depth, asWhite) //TODO This could be bad: if (isCurrentPlayerInCheck(board))
        } else {
          val moves = getNextMoveSummaries(board)

          if (moves.isEmpty) mate(board, depth, asWhite) // No legal moves. We already checked for stalemate above, so has to be mate.
          else {
            val filteredMoves = if (depth >= targetDepth) moves.filter(_._4) else moves

            // We/they can also choose not to do one of the filteredMoves (attacking), if there were other options
            val gameStoppedRightHere = if (depth >= targetDepth && filteredMoves.length < moves.length) {
              (board, gameEnd(board, depth, asWhite)) :: Nil
            } else Nil

            val currentScore = score(board, asWhite)
            val movesPar = if (depth >= parallelUntilDepth) filteredMoves else filteredMoves.par
            val games = movesPar.map { case (piece, square, newBoard, _) =>
              totalMovesMade.incrementAndGet()
              val game = bestGame(newBoard, depth + 1, asWhite, targetDepth, depthHardLimit)
              val scoreAchievedAt = if (game.score == currentScore) depth else game.scoreAchievedAt
              (newBoard, game.copy(scoreAchievedAt = scoreAchievedAt, moves = MoveHelpers.encodeMove(piece, square) :: game.moves))
            } ++ gameStoppedRightHere

            if (games.isEmpty) {
              if (depth >= targetDepth) gameEnd(board, depth, asWhite)
              else sys.error("This should never have happened")
            } else if (depth == 0) {
              bestGameByMaterialAndPosition(games, asWhite)
            } else bestGameByMaterialAndDepth(games, asWhite, board.isWhitesTurn)
          }
        }

        if (cachedGame(board.encoding, depth).isEmpty)
          visited.put(board.encoding, CachedGame(depth, game))

        game
      }
    }
  }
}
