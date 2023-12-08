package chess

import chess.board.Board

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong
import scala.collection.GenSeq
import scala.util.Random

case class Game(score: Int, moves: List[String])
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

  private def gameEnd(board: Board, asWhite: Boolean): Game = {
    totalGamesExplored.incrementAndGet()
    val mult = if (asWhite) 1 else -1
    val score = mult * (board.white.score - board.black.score)
    Game(score, Nil)
  }

  private def mate(board: Board, asWhite: Boolean): Game = {
    totalGamesExplored.incrementAndGet()
    val score = if (board.isWhitesTurn == asWhite) -999 else 999
    Game(score, Nil)
  }

  private def staleMate(): Game = {
    totalGamesExplored.incrementAndGet()
    Game(0, Nil)
  }

  private def cachedGame(boardEncoding: String, depth: Int): Option[CachedGame] =
    Option(visited.get(boardEncoding)).filter(_.cachedAtDepth <= depth)

  private def bestGameByMaterialAndPosition(games: GenSeq[(Board, Game)], asWhite: Boolean): Game = {
    val maxMaterialScore = games.map(_._2.score).max
    val gamesWithBestMaterialScore = games.filter(_._2.score == maxMaterialScore)
    val gamesAndPositionScores = gamesWithBestMaterialScore.map { case (board, game) =>
      (game, BestPosition.positionScore(board, asWhite))
    }
    val bestScore = gamesAndPositionScores.map(_._2).max
    val worstScore = gamesAndPositionScores.map(_._2).min
    println(s"Got ${gamesWithBestMaterialScore.size} games with the same material score.")
    println(s"Choosing the best game by position score, ranging from $worstScore to $bestScore.")
    Random.shuffle(gamesAndPositionScores.filter(_._2 == bestScore).toList).head._1
  }

  private def bestGameByMaterialAndDepth(games: GenSeq[(Board, Game)], asWhite: Boolean, isWhitesTurn: Boolean): Game = {
    if (asWhite == isWhitesTurn) games.maxBy(g => g._2.score * 1000 - g._2.moves.length)._2
    else games.minBy(g => g._2.score * 1000 + g._2.moves.length)._2
  }

  def bestGame(board: Board, depth: Int, asWhite: Boolean, targetDepth: Int, depthHardLimit: Int): Game = {
    cachedGame(board.encoding, depth) match {
      case Some(cachedGame) =>
        shortcircuitedGames.incrementAndGet()
        cachedGame.game
      case None => {
        val game = if (board.nextMoves.isEmpty) { // End of game, could be either mate or stalemate
          if (board.isCurrentPlayerInCheck) mate(board, asWhite)
          else staleMate()
        } else if (depth >= depthHardLimit) {
          gameEnd(board, asWhite) //TODO This could be bad: if (isCurrentPlayerInCheck(board))
        } else {
          val moves = board.nextMoves.flatMap { case (piece, square) =>
            val newBoard = board.movePiece(piece, square)

            if (!newBoard.isPreviousPlayerInCheck) {
              Some((piece, square, newBoard, board.getPiece(square.x, square.y).isDefined))
            } else None
          }

          if (moves.isEmpty) mate(board, asWhite) // No legal moves. We already checked for stalemate above, so has to be mate.
          else {
            val filteredMoves = if (depth >= targetDepth) moves.filter(_._4) else moves

            // We/they can also choose not to do one of the filteredMoves (attacking), if there were other options
            val gameStoppedRightHere = if (depth >= targetDepth && filteredMoves.length < moves.length) {
              (board, gameEnd(board, asWhite)) :: Nil
            } else Nil

            val movesPar = if (depth >= parallelUntilDepth) filteredMoves else filteredMoves.par
            val games = movesPar.map { case (piece, square, newBoard, _) =>
              totalMovesMade.incrementAndGet()
              val game = bestGame(newBoard, depth + 1, asWhite, targetDepth, depthHardLimit)
              (newBoard, game.copy(moves = MoveHelpers.encodeMove(piece, square) :: game.moves))
            } ++ gameStoppedRightHere

            if (games.isEmpty) {
              if (depth >= targetDepth) gameEnd(board, asWhite)
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
