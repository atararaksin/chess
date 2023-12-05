package chess

import board.Board

object Main {
  //var fen = "4k3/8/3p4/2n1q1r1/8/3PPPN1/2QB4/4KB2 w - - 0 1"
  //var fen = "4k3/8/3p4/2n1q1r1/5P2/3PP1N1/2QB4/4KB2 b - - 0 1"
  var fen = "r2qkbnr/1pp1pppp/p1n1b3/8/2p5/1PNPP3/P4PPP/R1BQKBNR w KQkq - 0 1"

  def main(args: Array[String]): Unit = {
    if (args.length > 0) fen = args(0)
    val targetDepth = if (args.length > 1) args(1).toInt else 2
    val depthHardLimit = if (args.length > 2) args(2).toInt else 12

    val board = Board.loadFen(fen)

    board.print()
    println(new String(board.encoding.toArray))

    println("")

    findBestMove(board, targetDepth, depthHardLimit)
//    printNextMoves(board)
  }

  def findBestMove(board: Board, targetDepth: Int, depthHardLimit: Int): Unit = {
    val mult = if (board.isWhitesTurn) 1 else -1
    val startScore = mult * (board.white.score - board.black.score)

    println(s"startScore: $startScore (white: ${board.white.score}, black: ${board.black.score})")

    val bestMove = new BestMove()
    val game = bestMove.findBestMove(board, targetDepth, depthHardLimit)

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
