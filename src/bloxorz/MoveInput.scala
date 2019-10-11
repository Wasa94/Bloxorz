package bloxorz

import java.io.File
import java.io.PrintWriter

object MoveInput {
  def moveInput(c: Char): Move = {
    c match {
      case 'w' | 'W' => Up
      case 'a' | 'A' => Left
      case 's' | 'S' => Down
      case 'd' | 'D' => Right
      case _ => throw new Error("Pogrešan unos!")
    }
  }

  def loadMoves(path: String): List[Char] = {
    val file = scala.io.Source.fromFile(path)
    val moves = file.getLines().toList
    if (moves.length != moves.flatten.length & moves.nonEmpty)
      throw new Error("Lista poteza nije validna!")
    else moves.map {
      case "u" => 'u'
      case "d" => 'd'
      case "l" => 'l'
      case "r" => 'r'
      case _ => throw new Error("Lista poteza nije validna!")
    }
  }

  def save(path: String, moves: String) = {
    val writer = new PrintWriter(new File(path))
    val lines = moves.toList
    lines.foreach(line => writer.println(line))
    writer.flush()
    writer.close()
  }

  def readCoordinate(): (Int, Int) = {
    try {
      print("x: ")
      val x = scala.io.StdIn.readInt()
      print("y: ")
      val y = scala.io.StdIn.readInt()
      (x, y)
    } catch {
      case _: NumberFormatException => println("Nevalidan unos!"); (-1, -1)
    }
  }
}
