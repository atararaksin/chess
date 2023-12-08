package chess.board

object BitSet {
  def addPieceIdToSet(pieceId: Int, bitSet: Int): Int =
    bitSet | (1 << pieceId)

  def removePieceIdFromSet(pieceId: Int, bitSet: Int): Int =
    bitSet & ~(1 << pieceId)

  def isPieceIdInSet(pieceId: Int, bitSet: Int): Boolean =
    (bitSet & (1 << pieceId)) != 0

  def unionSets(set1: Int, set2: Int): Int =
    set1 | set2
}
