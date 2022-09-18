package example

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
  def printAll: Unit = edges.foreach(x => {print(x); print(" -> ")}, println _)
}

// abstractin of puzzle state
case class State(bunny1: Node, bunny2: Node, dog: Node) {

}

object Main extends App {
  val nodes: List[Node] = 
    List("bone", "house", "boat", "tree", "carrot", "well", "flower")
      .map(s => Node(s))
  val edges: List[(Node, Node)] =
    List( ("bone", "house"),
          ("bone", "boat"),
          ("house", "bone"),
          ("house", "boat"),
          ("house", "tree"),
          ("boat", "house"),
          ("tree", "house"),
          ("tree", "well"),
          ("well", "tree"),
          ("well", "flower"),
          ("well", "carrot"),
          ("carrot", "tree"),
          ("carrot", "well"),
          ("flower", "boat"),
          ("flower", "well") ).map({ case (a, b) => (Node(a), Node(b)) })

  val graph = Graph(nodes, Edgemap(Map.empty)).addEdges(edges) 

  graph.printAll
}

