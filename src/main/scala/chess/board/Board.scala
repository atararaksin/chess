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

  lazy val nextMoves: List[(Piece, Square)] = {
    val player = if (isWhitesTurn) white else black
    for {
      piece <- player.pieces
      square <- piece.nextMoves(this)
    } yield (piece, square)
  }

  lazy val isPreviousPlayerInCheck: Boolean = {
    val previousPlayer = if (isWhitesTurn) black else white
    val king = previousPlayer.king
    MoveHelpers.isSquareUnderAttack(king.x, king.y, nextMoves)
  }

  lazy val isCurrentPlayerInCheck: Boolean = {
    this.copy(isWhitesTurn = !isWhitesTurn).isPreviousPlayerInCheck
  }

  def getPiece(x: Int, y: Int): Option[Piece] =
    squares(y * 8 + x)

  def movePiece(piece: Piece, square: Square): Board = {
    val x = square.x
    val y = square.y

    val updatedPiece = piece.move(x, y)

    var newWhite = white
    var newBlack = black

    if (piece.isWhite) newWhite = newWhite.movePiece(piece, updatedPiece)
    else newBlack = newBlack.movePiece(piece, updatedPiece)

    getPiece(x, y) match {
      case Some(other) if other.isWhite => newWhite = newWhite.removePiece(other)
      case Some(other) => newBlack = newBlack.removePiece(other)
      case None =>
    }

    val newSquares = squares.clone
    newSquares(y * 8 + x) = Some(updatedPiece)
    newSquares(piece.y * 8 + piece.x) = None

    this.copy(
      squares = newSquares, isWhitesTurn = !isWhitesTurn, white = newWhite, black = newBlack
    )
  }

  def print() = {
    val chars = for {
      y <- (0 to 7).toList
      x <- (0 to 7).toList
    } yield getPiece(x, y).map(_.reprChar).getOrElse('.')

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
      case (Some(char), i) => Some(Piece.fromChar(char, i % 8, i / 8))
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
