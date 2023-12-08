package chess.board

import chess.piece.{King, Piece}

case class PlayerState(isWhite: Boolean,
                       pieces: List[Piece],
                       canCastleK: Boolean,
                       canCastleQ: Boolean,
                       score: Int) {
  def removePiece(piece: Piece): PlayerState =
    this.copy(pieces = pieces.filterNot(_ eq piece), score = score - piece.value)

  def movePiece(piece: Piece, updatedPiece: Piece): PlayerState = {
    val newPieces = updatedPiece::pieces.filterNot(_ eq piece)
    this.copy(
      pieces = newPieces,
      score = score - piece.value + piece.value
    )
  }

  def king: Piece =
    pieces.find(_.isInstanceOf[King]).getOrElse(sys.error("No king on the board!"))
}
