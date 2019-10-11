package bloxorz

case class Map(tiles: List[List[Tile]]) {

  def findBlockPosition(tiles: List[List[Tile]]): List[Tile] = {
    tiles.flatten.filter(tile => tile.isOccupied)
  }

  def updateMap(move: Move): Map = {
    def findTileIndex(tile: Tile): (Int, Int) = {
      def findTileFlatIndex(tiles: List[Tile], tile: Tile, acc: Int): Int = {
        tiles match {
          case Nil => throw new Error("Polje nije pronađeno!")
          case x :: xs => if (x eq tile) acc else findTileFlatIndex(xs, tile, acc + 1)
        }
      }

      val numCols = tiles.head.length
      val flatIndex = findTileFlatIndex(tiles.flatten, tile, 0)
      (flatIndex / numCols, flatIndex % numCols)
    }

    def findNewBlockPosition(position: List[Tile], move: Move) = {
      def findUprightBlockNewPosition(pos: Tile, move: Move) = {
        val (row, col) = findTileIndex(pos)
        move match {
          case Up => tiles(row - 2)(col) :: tiles(row - 1)(col) :: Nil
          case Down => tiles(row + 1)(col) :: tiles(row + 2)(col) :: Nil
          case Left => tiles(row)(col - 2) :: tiles(row)(col - 1) :: Nil
          case Right => tiles(row)(col + 1) :: tiles(row)(col + 2) :: Nil
          case _ => throw new Error("Nemoguć potez!")
        }
      }

      def findLayingBlockNewPosition(position: List[Tile], move: Move) = {
        val (x1, y1) = findTileIndex(position.head)
        val (x2, y2) = findTileIndex(position(1))
        move match {
          case Up =>
            if (x1 == x2) tiles(x1 - 1)(y1) :: tiles(x2 - 1)(y2) :: Nil
            else {
              val x = if (x1 < x2) x1 else x2
              tiles(x - 1)(y1) :: Nil
            }
          case Down =>
            if (x1 == x2) tiles(x1 + 1)(y1) :: tiles(x2 + 1)(y2) :: Nil
            else {
              val x = if (x1 > x2) x1 else x2
              tiles(x + 1)(y1) :: Nil
            }
          case Left =>
            if (y1 == y2) tiles(x1)(y1 - 1) :: tiles(x2)(y2 - 1) :: Nil
            else {
              val y = if (y1 < y2) y1 else y2
              tiles(x1)(y - 1) :: Nil
            }
          case Right =>
            if (y1 == y2) tiles(x1)(y1 + 1) :: tiles(x2)(y2 + 1) :: Nil
            else {
              val y = if (y1 > y2) y1 else y2
              tiles(x1)(y + 1) :: Nil
            }
        }
      }

      if (position.length == 1) findUprightBlockNewPosition(position.head, move)
      else if (position.length == 2) findLayingBlockNewPosition(position, move)
      else throw new Error("Blok nije na validnoj poziciji!")
    }

    val blockPosList = findBlockPosition(tiles)
    val newBlockPosList = findNewBlockPosition(blockPosList, move)
    val toUpdate = blockPosList ::: newBlockPosList

    updateMapTiles(toUpdate, newBlockPosList.nonEmpty)
  }

  private def changeTile(toUpdate: Tile, f: Tile => Tile): Map = {
    Map(tiles.map(rows => rows.map(field =>
      if (field eq toUpdate) f(field)
      else field
    )))
  }

  private def getTileByPosition(coord: (Int, Int)): Tile = {
    val (x, y) = coord
    tiles(x)(y)
  }

  def convertTile(from: Tile, to: Tile, coord: (Int, Int) = (-1, -1)): Map = {
    val tile = if (from == StartTile(true) || from == EndTile(false)) from else getTileByPosition(coord)
    val newTile = if (from == tile || to == StartTile(true) || to == EndTile(false)) to else tile
    changeTile(tile, _ => newTile)
  }

  private def updateMapTiles(toUpdate: List[Tile], newBlockOrientation: Boolean): Map = {
    def invertTile(tile: Tile) = {
      val orientation = if (tile.isOccupied) false else newBlockOrientation

      tile match {
        case StandardTile(_) => StandardTile(orientation)
        case SpecialTile(_) => SpecialTile(orientation)
        case EmptyTile(_) => EmptyTile(orientation)
        case StartTile(_) => StartTile(orientation)
        case EndTile(_) => EndTile(orientation)
      }
    }

    toUpdate match {
      case Nil => this
      case x :: xs => changeTile(x, invertTile).updateMapTiles(xs, newBlockOrientation)
    }
  }

  def printRow(row: List[Tile], rowString: String): String = {
    row match {
      case Nil => rowString
      case field :: xs =>
        if (field.isOccupied)
          printRow(xs, rowString + "B")
        else
          field match {
            case StandardTile(_) => printRow(xs, rowString + "o")
            case SpecialTile(_) => printRow(xs, rowString + ".")
            case EmptyTile(_) => printRow(xs, rowString + "-")
            case StartTile(_) => printRow(xs, rowString + "o")
            case EndTile(_) => printRow(xs, rowString + "T")
          }
    }
  }

  def printMap(tiles: List[List[Tile]], row: String): String = {
    tiles match {
      case Nil => row
      case x :: xs => printMap(xs, printRow(x, row) + "\n")
    }
  }

  override def toString: String = {
    printMap(tiles, "")
  }
}
