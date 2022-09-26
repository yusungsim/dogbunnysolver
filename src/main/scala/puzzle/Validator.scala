package puzzle

import graph.{Graph, Node}

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
