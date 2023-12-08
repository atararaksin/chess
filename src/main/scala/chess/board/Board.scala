package chess.board

import chess.MoveHelpers
import chess.piece.Piece

case class Board(squares: Array[Option[Piece]],
                 isWhitesTurn: Boolean,
                 white: PlayerState,
                 black: PlayerState) {

  lazy val encoding: String = {
    val turnChar = if (isWhitesTurn) 'w' else 'b'
    val chars: Array[Char] = new Array(65)
    for (i <- 0 to 63) {
      val piece = squares(i)
      if (piece.isDefined) chars(i) = squares(i).get.reprChar
      else chars(i) = ' '
    }
    chars(64) = turnChar
    new String(chars)
  }

  lazy val nextMoves: List[(Piece, Array[Int])] = {
    val player = if (isWhitesTurn) white else black
    var piecesAndMoves: List[(Piece, Array[Int])] = Nil

    for (piece <- player.pieces) {
      val moves = piece.nextMoves(this)
      if (moves.nonEmpty) {
        piecesAndMoves ::= (piece, moves)
      }
    }
    piecesAndMoves
  }

  lazy val isPreviousPlayerInCheck: Boolean = {
    val previousPlayer = if (isWhitesTurn) black else white
    val king = previousPlayer.king
    MoveHelpers.isSquareUnderAttack(king.square, nextMoves)
  }

  lazy val isCurrentPlayerInCheck: Boolean = {
    this.copy(isWhitesTurn = !isWhitesTurn).isPreviousPlayerInCheck
  }

  def getPiece(square: Int): Option[Piece] =
    squares(square)

  def movePiece(piece: Piece, square: Int): Board = {
    val updatedPiece = piece.move(square)

    var newWhite = white
    var newBlack = black

    if (piece.isWhite) newWhite = newWhite.movePiece(piece, updatedPiece)
    else newBlack = newBlack.movePiece(piece, updatedPiece)

    getPiece(square) match {
      case Some(other) if other.isWhite => newWhite = newWhite.removePiece(other)
      case Some(other) => newBlack = newBlack.removePiece(other)
      case None =>
    }

    val newSquares = squares.clone
    newSquares(square) = Some(updatedPiece)
    newSquares(piece.square) = None

    this.copy(
      squares = newSquares, isWhitesTurn = !isWhitesTurn, white = newWhite, black = newBlack
    )
  }

  def print() = {
    val chars = for {
      square <- 0 to 63
    } yield getPiece(square).map(_.reprChar).getOrElse('.')

    val board = chars.grouped(8).map(_.mkString("  ")).mkString("\n")

    println(board)
  }
}

object Board {
  def loadFen(fen: String): Board = {
    val split = fen.split(' ').map(_.trim)

    val squaresRows = split(0).split('/')
    val turn = split(1)
    val castle = split(2)

    def readPieceChar(char: Char): List[Option[Char]] =
      if (char.isDigit) List.fill(char.toString.toInt)(None)
      else List(Some(char))

    val piecesChars = squaresRows.mkString.toCharArray.flatMap(readPieceChar)
    val squares = piecesChars.zipWithIndex.map {
      case (Some(char), i) => Some(Piece.fromChar(char, i))
      case (None, _) => None
    }

    if (squares.size != 64) sys.error(s"FEN contained ${squares.size} squares instead of 64")

    val isWhitesTurn = turn.equals("w")
    val canWhiteCastleK = castle.contains('K')
    val canWhiteCastleQ = castle.contains('Q')
    val canBlackCastleK = castle.contains('k')
    val canBlackCastleQ = castle.contains('q')

    val whitePieces = squares.flatten.filter(_.isWhite).toList
    val blackPieces = squares.flatten.filterNot(_.isWhite).toList

    val white = PlayerState(
      isWhite = true, whitePieces,
      canCastleK = canWhiteCastleK, canCastleQ = canWhiteCastleQ,
      score = whitePieces.map(_.value).sum
    )
    val black = PlayerState(
      isWhite = false, blackPieces,
      canCastleK = canBlackCastleK, canCastleQ = canBlackCastleQ,
      score = blackPieces.map(_.value).sum
    )

    Board(
      squares = squares,
      isWhitesTurn = isWhitesTurn,
      white = white,
      black = black
    )
  }
}
