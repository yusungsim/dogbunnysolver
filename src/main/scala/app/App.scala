package app

import graph.{Node, Graph, Edgemap}
import puzzle.{State, Validator, Generator, Solver}


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

  val initState = State(Node("house"), Node("boat"), Node("tree"))
  val goalState = State(Node("carrot"), Node("carrot"), Node("bone"))
  val validr = Validator(graph)
  val genr = Generator(graph, validr)
  val solver = Solver(validr, genr)
  val solution: List[State] = solver(initState, goalState)

  println(">>> Solution <<<")
  solution.foreach(_.print)
}

