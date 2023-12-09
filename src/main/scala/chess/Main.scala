package chess

import board.Board

import scala.annotation.tailrec

object Main {
  var fen = "4k3/8/3p4/2n1q1r1/8/3PPPN1/2QB4/4KB2 b - - 0 1"
//  var fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

  var maxMillis = 3000

  def main(args: Array[String]): Unit = {
    if (args.length > 0) fen = args(0)
    if (args.length > 1) maxMillis = args(1).toInt

    val board = Board.loadFen(fen)

    board.print()

    println("")

      findBestMove(board)
//    printNextMoves(board)
  }

  def findBestMove(board: Board): Unit = {
    val mult = if (board.isWhitesTurn) 1 else -1
    val startScore = mult * (board.white.score - board.black.score)

    println(s"startScore: $startScore (white: ${board.white.score}, black: ${board.black.score})")

    val bestMove = new BestMove()

    @tailrec
    def tryWithDepth(targetDepth: Int): Game = {
      println("")
      println(s"Trying with targetDepth=$targetDepth")
      val game = bestMove.findBestMove(board, targetDepth, targetDepth + 6)
      println(s"Got result in ${bestMove.elapsedMillis} millis: ${game.moves.head}")

      if (targetDepth > 9) game
      else if (bestMove.elapsedMillis > maxMillis) game
      else {
        val newTargetDepth = targetDepth + 1
        tryWithDepth(newTargetDepth)
      }
    }

    val game = tryWithDepth(1)

    println("")
    println(s"endScore:   ${game.score}")
    val scoreDiff = game.score - mult * (board.white.score - board.black.score)
    println(s"scoreDiff:  $scoreDiff")
    println(s"""Moves:      ${game.moves.mkString(", ")}""")

    println("")
    println(s"Time elapsed:             ${bestMove.elapsedMillis} millis.")
    println(s"Total games explored:     ${bestMove.totalGamesExplored.get()}")
    println(s"Total moves made:         ${bestMove.totalMovesMade.get()}")
    println(s"Shortcircuited positions: ${bestMove.shortcircuitedGames.get()}")
  }



  def printNextMoves(board: Board): Unit = {
    val player = if (board.isWhitesTurn) board.white else board.black

    player.pieces.foreach { piece =>
      piece.nextMoves(board).foreach { square =>
        MoveHelpers.encodeMove(piece, square)
        val newBoard = board.movePiece(piece, square)
        newBoard.print()
        println("")
      }
      println("")
    }
  }
}
