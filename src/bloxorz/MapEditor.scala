package bloxorz

object MapEditor {
  def readMapFromFile(path: String): Map = {
    val file = scala.io.Source.fromFile(path)
    val lines = file.getLines().toList
    Map(convertToTiles(padMap(lines)))
  }

  private def convertToTiles(lines: List[String]): List[List[Tile]] = {
    def createListOfTiles(s: List[Char]): List[Tile] = {
      s match {
        case 'o' :: xs => StandardTile(false) :: createListOfTiles(xs)
        case '.' :: xs => SpecialTile(false) :: createListOfTiles(xs)
        case '-' :: xs => EmptyTile(false) :: createListOfTiles(xs)
        case 'S' :: xs => StartTile(true) :: createListOfTiles(xs)
        case 'T' :: xs => EndTile(false) :: createListOfTiles(xs)
        case Nil => Nil
        case _ => throw new Error("Incorrect level format " + s.head)
      }
    }

    lines match {
      case Nil => Nil
      case x :: xs => createListOfTiles(x.toList) :: convertToTiles(xs)
    }
  }

  private def padMap(rows: List[String]): List[String] = {
    val maxLength = rows.maxBy(_.length).length
    val padLine = "-" * (maxLength + 4)

    val padList = padLine :: padLine :: Nil

    padList ::: rows.map(row => "--" + row + "-" * (maxLength - row.length + 2)) ::: padList
  }

  private def getStartTile(map: Map): Tile = map.tiles.flatten.find(_ == StartTile(true)).get

  private def getEndTile(map: Map): Tile = map.tiles.flatten.find(_ == EndTile(false)).get

  private def printMenu(): Unit = {
    println("Editor: ")
    println()
    println("1. Uklanjanje ploče")
    println("2. Dodavanje ploče")
    println("3. Zamena obične ploče specijalnom pločom")
    println("4. Zamena specijalne ploče običnom pločom")
    println("5. Promena startne pozicije")
    println("6. Promena krajnje pozicije")
    println("7. Inverzija (start <-> cilj)")
    println("8. Zamena (specijalne -> obične)")
    println("9. Filtriranje polja")
    println("10. Kompozicija operacija")
    println("11. Sekvenca operacija")
    println("12. Napravi kompoziciju operacija")
    println("13. Napravi sekvencu operacija")
    println("0. Kraj editovanja")
    println()
    println("Vaš izbor? ")

  }

  def edit(map: Map): Map = {
    def standardToEmpty(map: Map) = {
      println("Uklanjanje ploče")
      map.convertTile(StandardTile(false), EmptyTile(false), MoveInput.readCoordinate())
    }

    def emptyToStandard(map: Map) = {
      println("Dodavanje ploče")
      map.convertTile(EmptyTile(false), StandardTile(false), MoveInput.readCoordinate())
    }

    def standardToSpecial(map: Map) = {
      println("Zamena obične ploče specijalnom pločom")
      map.convertTile(StandardTile(false), SpecialTile(false), MoveInput.readCoordinate())
    }

    def specialToStandard(map: Map) = {
      println("Zamena specijalne ploče običnom pločom")
      map.convertTile(SpecialTile(false), StandardTile(false), MoveInput.readCoordinate())
    }

    def moveStart(map: Map) = {
      println("Promena startne pozicije")
      map.convertTile(StandardTile(false), StartTile(true), MoveInput.readCoordinate()).convertTile(getStartTile(map), StandardTile(false))
    }

    def moveEnd(map: Map) = {
      println("Promena krajnje pozicije")
      map.convertTile(StandardTile(false), EndTile(false), MoveInput.readCoordinate()).convertTile(getEndTile(map), StandardTile(false))
    }

    def invertMap(map: Map) = {
      println("Inverzija (start <-> cilj)")
      map.convertTile(getEndTile(map), StartTile(true)).convertTile(getStartTile(map), EndTile(false))
    }

    def swapMap(map: Map) = {
      println("Zamena (specijalne -> obične)")
      def convertTileToStandard(tile: Tile) = {
        if (tile == SpecialTile(false)) StandardTile(false)
        else tile
      }

      Map(for (row <- map.tiles) yield row.map(tile => convertTileToStandard(tile)))
    }

    def filterMap(map: Map) = {
      println("Filtriranje polja")
      def hasSpecialNeighbours(n: Int, coord: (Int, Int)): Boolean = {
        def isSpecialTile(x: Int, y: Int) = {
          map.tiles(x)(y) == SpecialTile(false)
        }

        val (x, y) = coord
        val minX = math.min(x - n, 0)
        val minY = math.min(y - n, 0)
        val maxY = math.min(y + n, map.tiles.head.length - 1)
        val maxX = math.min(x + n, map.tiles.length - 1)

        if (n <= 0)
          false
        else if (isSpecialTile(minX, y) || isSpecialTile(maxX, y)
          || isSpecialTile(x, minY) || isSpecialTile(x, maxY))
          true
        else
          hasSpecialNeighbours(n - 1, coord)
      }

      val coord = MoveInput.readCoordinate()
      print("N: ")
      val n = scala.io.StdIn.readInt()
      if (hasSpecialNeighbours(n, coord))
        map.convertTile(SpecialTile(false), StandardTile(false), coord)
      else
        map
    }

    def compose(operations: List[Map => Map], map: Map) = {
      println("Kompozicija operacija")
      if(operations.length <= 1) map
      else operations.reduce((a, b) => a.compose(b))(map)
    }

    def sequential(operations: List[Map => Map], map: Map) = {
      println("Sekvenca operacija")
      if(operations.length <= 1) map
      else operations.reduce((a, b) => a.andThen(b))(map)
    }

    def makeListOfOperations(): List[Map => Map] = {
      try {
        val operations = scala.io.StdIn.readLine("Operacije koje ulaze u listu (1-9): ").split(" ").map(_.toInt - 1)
        val allOpts: List[Map => Map] = List(standardToEmpty, emptyToStandard, standardToSpecial, specialToStandard,
          moveStart, moveEnd, invertMap, swapMap, filterMap)

        allOpts.zipWithIndex
          .filter { case (_, index) => operations.contains(index) }
          .map(_._1)
      }
      catch {
        case _: Error =>
          println("Greska u unosu!")
          List()
      }
    }

    def Menu(map: Map, composition: List[Map => Map], sequence: List[Map => Map]): Map = {
      println(map)
      printMenu()
      try {
        val opt = scala.io.StdIn.readInt()
        opt match {
          case 1 => return Menu(standardToEmpty(map), composition, sequence)
          case 2 => return Menu(emptyToStandard(map), composition, sequence)
          case 3 => return Menu(standardToSpecial(map), composition, sequence)
          case 4 => return Menu(specialToStandard(map), composition, sequence)
          case 5 => return Menu(moveStart(map), composition, sequence)
          case 6 => return Menu(moveEnd(map), composition, sequence)
          case 7 => return Menu(invertMap(map), composition, sequence)
          case 8 => return Menu(swapMap(map), composition, sequence)
          case 9 => return Menu(filterMap(map), composition, sequence)
          case 10 => return Menu(compose(composition, map), composition, sequence)
          case 11 => return Menu(sequential(sequence, map), composition, sequence)
          case 12 => return Menu(map, makeListOfOperations(), sequence)
          case 13 => return Menu(map, composition, makeListOfOperations())
          case 0 =>
        }
      } catch {
        case _: NumberFormatException => println("Nevalidna opcija!")
        case _: IndexOutOfBoundsException => println("Uneto polje ne postoji!")
        case e: Error => println(e.toString)
      }
      map
    }
    //compose(List(moveStart, invertMap))
    Menu(map, List(), List())
  }
}
