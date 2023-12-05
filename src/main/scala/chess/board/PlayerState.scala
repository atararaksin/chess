package chess.board

import chess.piece.{King, Piece}

case class PlayerState(isWhite: Boolean,
                       pieces: Set[Piece],
                       canCastleK: Boolean,
                       canCastleQ: Boolean,
                       score: Int) {
  def removePiece(piece: Piece): PlayerState =
    this.copy(pieces = pieces.filterNot(_ eq piece), score = score - piece.value)

  def movePiece(piece: Piece, updatedPiece: Piece): PlayerState = {
    val newPieces = pieces.filterNot(_ eq piece) + updatedPiece
    this.copy(pieces = newPieces)
  }

  def king: Piece =
    pieces.find(_.isInstanceOf[King]).getOrElse(sys.error("No king on the board!"))
}
