package graph

// Abstraction of general graph
case class Node(name: String)
case class Edgemap[T](map: Map[T, List[T]]) {
  def addEdge(s: T, e: T): Edgemap[T] = {
    val newMap = 
      if (map.keySet contains s) {
        val orgEnds = map(s)
        map + (s -> (orgEnds :+ e))
      } else {
        map + (s -> List(e))
      }
    Edgemap(newMap)
  }

  // todo : better name for this
  def getNextNodes(s: T): List[T] = map.getOrElse(s, List())

  // abstraction for print all
  def foreach(f1: T => Unit, f2: T => Unit) = 
    map.foreach({ 
      case (x, ys) => ys.foreach(y => {f1(x); f2(y)}) 
    })
}

case class Graph(nodes: List[Node], edges: Edgemap[Node]) { 
  def addNode(n: Node) = Graph(nodes :+ n, edges)
  def addNodes(ns: List[Node]) = Graph(nodes ++ ns, edges)
  def addEdge(s: Node, e: Node) = {
    if (! ((nodes contains s) && (nodes contains e)) ) {
       throw new Exception("invalid nodes")
    }
    Graph(nodes, edges.addEdge(s, e))
  }
  def addEdges(ns: List[(Node, Node)]) = {
    val newEdgemap =
      ns.foldLeft(edges)((m, nn) => {
        nn match {
          case (s, e) => m.addEdge(s, e)
        }
      })
    Graph(nodes, newEdgemap)
  }

  // todo: better name for this
  def getNextNodes(s: Node): List[Node] = edges.getNextNodes(s)

  // printer
  def printAll: Unit = edges.foreach(x => {print(x); print(" -> ")}, println _)
}
