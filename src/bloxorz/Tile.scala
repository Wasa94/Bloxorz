package bloxorz

sealed abstract class Tile {
  def isOccupied: Boolean
}

case class StandardTile(isOccupied: Boolean) extends Tile

case class StartTile(isOccupied: Boolean) extends Tile

case class EndTile(isOccupied: Boolean) extends Tile

case class EmptyTile(isOccupied: Boolean) extends Tile

case class SpecialTile(isOccupied: Boolean) extends Tile
