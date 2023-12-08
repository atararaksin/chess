package chess.board

case class PlayerState(isWhite: Boolean,
                       pieces: List[Int],
                       canCastleK: Boolean,
                       canCastleQ: Boolean,
                       score: Int)
