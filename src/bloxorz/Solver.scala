package bloxorz

case class Vertex(state: Map, moves: String)

case class Graph(vertices: List[Vertex]) {
  def getNextVertex(visited: List[Vertex]): Vertex = {
    vertices.find(!visited.contains(_)) match {
      case None => throw new Error("Mapa se ne može rešiti!")
      case Some(n) => n
    }
  }
}

object Solver {
  private def getNeighbour(v: Vertex, move: Move): Vertex = {
    val nextState = v.state.updateMap(move)

    val c = move match {
      case Up => 'u'
      case Down => 'd'
      case Left => 'l'
      case Right => 'r'
    }

    GameDef.checkGameState(nextState) match {
      case -1 => null
      case _ => Vertex(nextState, v.moves + c)
    }
  }

  private def visitVertex(v: Vertex): List[Vertex] = {
    (getNeighbour(v, Up) :: getNeighbour(v, Right) :: getNeighbour(v, Down) :: getNeighbour(v, Left) :: Nil).filter(_ != null)
  }

  private def findPath(graph: Graph, vertex: Vertex, visited: List[Vertex]): Graph = {
    if (!visited.contains(vertex)) {
      val neighbours = visitVertex(vertex)
      val notVisitedNeighbours = neighbours.filter(v => graph.vertices.forall(v.state != _.state))
      val newGraph = Graph(graph.vertices ::: notVisitedNeighbours)
      val newVisited = vertex :: visited

      if (!notVisitedNeighbours.exists(v => GameDef.checkGameState(v.state) == 1)) findPath(newGraph, newGraph.getNextVertex(newVisited), newVisited)
      else newGraph
    }
    else graph
  }

  def solve(map: Map): String = {
    val start = Vertex(map, "")
    val g = findPath(Graph(List(start)), start, List())
    g.vertices.find(v => GameDef.checkGameState(v.state) == 1).get.moves
  }
}
