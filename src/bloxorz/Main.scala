package bloxorz

import java.io.{File, PrintWriter}

object Main {
  def printMenu(): Unit = {
    println()
    println("1. Nova igra")
    println("2. Učitaj rešenje")
    println("3. Rešenje")
    println("4. Editor")
    println("5. Prikaži mapu")
    println("0. Nazad")
    println()
    println("Vaš izbor?")
  }

  def main(args: Array[String]): Unit = {
    try {
      do {
        println()
        println("~~~BLOXORZ~~~")
        println("1. Učitaj mapu")
        println("0. Kraj")
        println()
        println("Vaš izbor?")

        val opt = scala.io.StdIn.readInt()
        val map = opt match {
          case 1 => MapEditor.readMapFromFile(scala.io.StdIn.readLine("Putanja do fajla: "))
          case 0 => return
          case _ => null
        }

        if (map != null) while (menu(map) != 0) {}
        else println("Nepostojeća opcija!")

      } while (true)
    } catch {
      case e: Error => println(e)
      case _: NumberFormatException => println("Greška u unosu!")
    }
  }

  def menu(map: Map): Int = {
    printMenu()
    try {
      val opt = scala.io.StdIn.readInt()
      opt match {
        case 1 =>
          val result = GameDef.play(map)
          result match {
            case 'L' => println("PORAZ")
            case 'W' => println("POBEDA")
            case 'Q' => return 0
            case 'M' => return 1
          }
        case 2 =>
          val moves = MoveInput.loadMoves(scala.io.StdIn.readLine("Putanja do fajla sa rešenjem: "))
          val result = GameDef.loadSolution(map, moves)
          println(result)
        case 3 =>
          val result = Solver.solve(map)
          println("Rešenje: " + result)
          val file = scala.io.StdIn.readLine("Putanja do fajla za čuvanje rešenja (prazan string da se ne sačuva): ")
          if (file != "") MoveInput.save(file, result)
        case 4 =>
          val newMap = MapEditor.edit(map)
          val file = scala.io.StdIn.readLine("Putanja do fajla za čuvanje nove mape (prazan string da se ne sačuva): ")
          if (file != "") {
            saveMap(file, newMap.toString().replace('B', 'S'))
            return 0
          }
        case 5 => println(map)
        case 0 =>
        case _ => println("Nepostojeća opcija!")
      }
      opt
    } catch {
      case e: Error =>
        println(e)
        1
      case _: NumberFormatException =>
        println("Greška u unosu!")
        1
    }
  }

  def saveMap(path: String, data: String): Unit = {
    val writer = new PrintWriter(new File(path))
    writer.println(data)
    writer.flush()
    writer.close()
  }
}
