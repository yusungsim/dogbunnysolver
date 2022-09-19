package example
import scala.annotation.tailrec

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

// abstraction of puzzle state
case class State(b1: Node, b2: Node, dog: Node) {
  def moveBunny1(n: Node): State = State(n, b2, dog)    
  def moveBunny2(n: Node): State = State(b1, n, dog)    
  def moveDog(n: Node): State = State(b1, b2, n)    
  def print = println(s"Bunny1: ${b1.name} | Bunny2: ${b2.name} | Dog: ${dog.name}")
}

// validates certain move is possible
case class Validator(graph: Graph) {
  def checkBunny1(state: State)(from: Node, to: Node): Boolean = 
    checkMove(state)(from, to)
  def checkBunny2(state: State)(from: Node, to: Node): Boolean =
    checkMove(state)(from, to)
  def checkDog(state: State)(from: Node, to: Node): Boolean =
    checkMove(state)(from, to)
  // true : valid, false: invalid
  def checkMove(state: State)(from: Node, to: Node): Boolean = {
    // hand-crafted actual rules.
    // see the (puzzle)[http://www.dogbunnypuzzle.com/]
    state match {
      case State(b1, b2, dog) => {
        val takenNodes = List(b1, b2, dog)
        (from.name, to.name) match {
          case ("bone", "house") | ("house", "bone") => {
            takenNodes contains Node("carrot")
          }
          case ("house", "boat") | ("boat", "house") => {
            takenNodes contains Node("tree")
          }
          case ("house", "tree") | ("tree", "house") => {
            (takenNodes contains Node("bone")) && 
            (takenNodes contains Node("flower"))
          }
          case ("carrot", "well") | ("well", "carrot") => {
            !(takenNodes contains Node("bone"))
          }
          case _ => true
        }
      }
    }
  }
}

// generates next possible states from given state 
case class Generator(graph: Graph, validr: Validator) {
  def genNextStates(state: State): List[State] = {
    state match {
      case State(b1, b2, dog) => {
        // first gen moving bunny1
        val nextBunny1Nodes = graph.getNextNodes(b1)
        // for connected next nodes of current bunny 1 node,
        val nextBunny1States: List[State] = 
          // filter only valid next nodes
          nextBunny1Nodes.filter(next => validr.checkBunny1(state)(b1, next))
          // then genrate states that moves bunny1 to that nodes.
            .map(next => state.moveBunny1(next))

        // second gen moving bunny2
        // similar logic
        val nextBunny2Nodes = graph.getNextNodes(b2)
        val nextBunny2States: List[State] = 
          nextBunny2Nodes.filter(next => validr.checkBunny2(state)(b2, next))
            .map(next => state.moveBunny2(next))

        // third gen moving dog
        // similar logiv
        val nextDogNodes = graph.getNextNodes(dog)
        val nextDogStates: List[State] = 
          nextDogNodes.filter(next => validr.checkDog(state)(dog, next))
            .map(next => state.moveDog(next))

        // aggregate all possible states
        nextBunny1States ++ nextBunny2States ++ nextDogStates
      }
    }
  }
}

// solves the puzzle with Validator and Generator
case class Solver(check: Validator, gen: Generator){
  def apply(init: State, goal: State): List[State] = {
    val initQueue = List((init, 0))
    val rawsol = solveWithQueue(goal, initQueue, 0)
    init :: rawsol.reverse
  }

  def backtrack(queue: List[(State, Int)], id: Int): List[State] = {
    queue(id) match {
      case (curState, prevId) => {
        if (prevId == 0) {
          List(curState)
        } else {
          curState :: backtrack(queue, prevId) 
        }
      }
    }
  }

  // queue-based naive solver
  def solveWithQueue(
    goal: State, 
    // list-based queue. 
    queue: List[(State, Int)], 
    // index of head of queue. 
    head: Int
  ): List[State] = {
    // pops a state, check if it's a goal state
    queue(head) match {
      case (curState, _) => {
        // if curState is goal, end the search.
        // backtrack to produce the actual trace
        if (curState == goal) {
          backtrack(queue, head)
        // if curState is not a goal,
        // generate next states and add to the queue.
        // then continue solving with next queue element.
        // should avoid adding redundent states
        } else {
          val nextStates = gen.genNextStates(curState)
          val newQueue = 
            queue ++ nextStates
              .filter(x => {
                val queueOnlyStates = queue.map({ case (s, i) => s })
                !(queueOnlyStates contains x)
              })
              .map(x => (x, head))
          solveWithQueue(goal, newQueue, head+1) // todo: TCO
        }
      }
    }
  }
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

  val initState = State(Node("house"), Node("boat"), Node("tree"))
  val goalState = State(Node("carrot"), Node("carrot"), Node("bone"))
  val validr = Validator(graph)
  val genr = Generator(graph, validr)
  val solver = Solver(validr, genr)
  val solution: List[State] = solver(initState, goalState)

  println(">>> Solution <<<")
  solution.foreach(_.print)
}

