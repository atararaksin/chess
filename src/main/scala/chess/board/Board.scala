package chess.board

import chess.MoveHelpers
import chess.piece.Piece

case class Board(squares: Array[Option[Int]],
                 dependentPieces: Array[Int], // Each Int is representation of 32 Boolean values for each pieceId
                 pieces: Array[Piece], // Index is pieceId
                 isWhitesTurn: Boolean,
                 white: PlayerState,
                 black: PlayerState) {

  lazy val encoding: String = {
    val turnChar = if (isWhitesTurn) 'w' else 'b'
    val chars: Array[Char] = new Array(65)
    for (i <- 0 to 63) {
      val pieceId = squares(i)
      if (pieceId.isDefined) chars(i) = pieces(pieceId.get).reprChar
      else chars(i) = ' '
    }
    chars(64) = turnChar
    new String(chars)
  }

  lazy val nextMoves: List[(Piece, Square)] = {
    val player = if (isWhitesTurn) white else black
    for {
      pieceId <- player.pieces
      piece = pieces(pieceId)
      square <- piece.moves
    } yield (piece, square)
  }

  lazy val isPreviousPlayerInCheck: Boolean = {
    val king = getKing(!isWhitesTurn)
    MoveHelpers.isSquareUnderAttack(king.x, king.y, nextMoves)
  }

  lazy val isCurrentPlayerInCheck: Boolean = {
    this.copy(isWhitesTurn = !isWhitesTurn).isPreviousPlayerInCheck // TODO can we do better now?
  }

  def getKing(isWhite: Boolean): Piece =
    if (isWhite) pieces(0) else pieces(16)

  def getPiece(x: Int, y: Int): Option[Piece] =
    squares(y * 8 + x).map(pieces)

  def movePiece(piece: Piece, square: Square): Board = {
    val toX = square.x
    val toY = square.y

    val fromSquare = piece.y * 8 + piece.x
    val toSquare = square.y * 8 + square.x

    val eatenPiece = getPiece(toX, toY)

    val newSquares = squares.clone
    newSquares(fromSquare) = None
    newSquares(toSquare) = Some(piece.id)

    val (newPieces, newDependentPieces) = calculateNewPieces(piece, eatenPiece, fromSquare, toSquare, toX, toY, newSquares)

    val newWhite = newPlayer(white, eatenPiece)
    val newBlack = newPlayer(black, eatenPiece)

    this.copy(
      squares = newSquares,
      dependentPieces = newDependentPieces,
      pieces = newPieces,
      isWhitesTurn = !isWhitesTurn,
      white = newWhite,
      black = newBlack
    )
  }

  private def calculateNewPieces(piece: Piece, eatenPiece: Option[Piece], fromSquare: Int, toSquare: Int,
                                 toX: Int, toY: Int, newSquares: Array[Option[Int]]) = {
    val newPieces = pieces.clone()
    val newDependentPieces = dependentPieces.clone()

    val allAffectedPieceIds = BitSet.unionSets(dependentPieces(fromSquare), dependentPieces(toSquare)) // should always contain piece.id too

    for (pieceId <- 0 to 31) {
      if (BitSet.isPieceIdInSet(pieceId, allAffectedPieceIds)) {
        val oldPiece = pieces(pieceId)
        removePieceFromDependentPieces(oldPiece, newDependentPieces)
        val updatedPiece = if (pieceId == piece.id) {
          Piece.fromOldPiece(piece, toX, toY, newSquares)
        } else {
          Piece.fromOldPiece(oldPiece, piece.x, piece.y, newSquares)
        }
        addPieceToDependentPieces(updatedPiece, newDependentPieces)
        newPieces(pieceId) = updatedPiece
      }
    }
    eatenPiece.foreach(eaten => removePieceFromDependentPieces(eaten, newDependentPieces))

    (newPieces, newDependentPieces)
  }
  private def removePieceFromDependentPieces(piece: Piece, depPieces: Array[Int]) =
    piece.dependentSquares.foreach { s =>
      val i = s.y * 8 + s.x
      depPieces(i) = BitSet.removePieceIdFromSet(piece.id, depPieces(i))
    }

  private def addPieceToDependentPieces(piece: Piece, depPieces: Array[Int]) =
    piece.dependentSquares.foreach { s =>
      val i = s.y * 8 + s.x
      depPieces(i) = BitSet.addPieceIdToSet(piece.id, depPieces(i))
    }

  private def newPlayer(oldPlayer: PlayerState, eatenPiece: Option[Piece]) = {
    eatenPiece.filter(_.isWhite == oldPlayer.isWhite).map { eaten =>
      oldPlayer.copy(
        pieces = oldPlayer.pieces.filterNot(_ == eaten.id),
        score = oldPlayer.score - eaten.value
      )
    }.getOrElse(oldPlayer)
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
    val allPieces = piecesChars.zipWithIndex.flatMap {
      case (Some(char), i) => Some((char, i))
      case (None, _) => None
    }.sortBy { p =>
      // Let kings be always index 0 and 1 for quick discoverability,
      // and the rest be ordered by is White
      if (p._1 == 'k') 0 else 1
    }

    val squares = Array.fill[Option[Int]](64)(None)
    val pieces = new Array[Piece](32)
    var whitePieces = List[Int]()
    var blackPieces = List[Int]()
    var whiteId = 0
    var blackId = 16
    allPieces.foreach { case (char, square) =>
      if (char.isUpper) { // White
        val piece = Piece.fromChar(whiteId, char, square % 8, square / 8)
        pieces(whiteId) = piece
        squares(piece.y * 8 + piece.x) = Some(whiteId)
        whitePieces ::= whiteId
        whiteId += 1
      } else {
        val piece = Piece.fromChar(blackId, char, square % 8, square / 8)
        pieces(blackId) = piece
        squares(piece.y * 8 + piece.x) = Some(blackId)
        blackPieces ::= blackId
        blackId += 1
      }
    }

    val isWhitesTurn = turn.equals("w")
    val canWhiteCastleK = castle.contains('K')
    val canWhiteCastleQ = castle.contains('Q')
    val canBlackCastleK = castle.contains('k')
    val canBlackCastleQ = castle.contains('q')

    val white = PlayerState(
      isWhite = true, whitePieces,
      canCastleK = canWhiteCastleK, canCastleQ = canWhiteCastleQ,
      score = whitePieces.map(pieces(_).value).sum
    )
    val black = PlayerState(
      isWhite = false, blackPieces,
      canCastleK = canBlackCastleK, canCastleQ = canBlackCastleQ,
      score = blackPieces.map(pieces(_).value).sum
    )

    (whitePieces ++ blackPieces).foreach { pieceId =>
      val p = pieces(pieceId)
      pieces(pieceId) = Piece.fromOldPiece(p, p.x, p.y, squares)
    }

    val dependentPieces: Array[Int] = Array.fill(64)(0)
    for {
      pieceId <- whitePieces ++ blackPieces
      square <- pieces(pieceId).dependentSquares
    } dependentPieces(square.y * 8 + square.x) = BitSet.addPieceIdToSet(pieceId, dependentPieces(square.y * 8 + square.x))

    Board(
      squares = squares,
      dependentPieces = dependentPieces,
      pieces = pieces,
      isWhitesTurn = isWhitesTurn,
      white = white,
      black = black
    )
  }
}