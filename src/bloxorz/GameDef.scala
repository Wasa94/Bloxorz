package bloxorz

object GameDef {

  def checkGameState(map: Map): Int = {
    val block = map.findBlockPosition(map.tiles)
    block match {
      case List(EndTile(true)) => 1
      case List(SpecialTile(true)) => -1
      case _ if block.contains(EmptyTile(true)) => -1
      case _ => 0
    }
  }

  def play(map: Map): Char = {
    println(map)
    checkGameState(map) match {
      case -1 => return 'L'
      case 1 => return 'W'
      case _ =>
    }

    println("Kretanje W/A/S/D")
    println("Kraj Q")
    println("Meni M")
    val input = scala.io.StdIn.readChar()

    if (input == 'q' || input == 'Q' || input == 'm' || input == 'M') return input.toUpper
    try {
      val newMap = map.updateMap(MoveInput.moveInput(input))
      play(newMap)
    } catch {
      case _: IndexOutOfBoundsException =>
        println("Greška u igri!")
        'L'
    }
  }

  def loadSolution(map: Map, moves: List[Char]): Map = {
    def executeMoves(map: Map, moves: List[Char]): Map = {
      moves match {
        case Nil => map
        case x :: xs =>
          try {
            val move = x match {
              case 'd' => Down
              case 'u' => Up
              case 'r' => Right
              case 'l' => Left
            }
            val newMap = map.updateMap(move)
            if (checkGameState(map) == 0) executeMoves(newMap, xs)
            else map
          } catch {
            case _: IndexOutOfBoundsException =>
              println("Greška u izvršavanju sekvence poteza!")
              map
          }
      }
    }

    executeMoves(map, moves)
  }

}

