package floor2024.oop

import scala.collection.mutable

object OOP5 {
  /*
   * You have been given a case class Edge and Node,
   * and within the class Graph there is a to be identified function.
   * What does funA do and what does it return?
   */

  case class Edge(source: Node, dest: Node, weight: Int)
  case class Node(name: String)

  class Graph(val nodes: List[Node], val edges: List[Edge]) {
    // this is a list with all neighbours of each Node
    private val adjacencyList: Map[Node, List[Edge]] = {
      val adjacencyMap = mutable.Map[Node, List[Edge]]()

      for (node <- nodes) {
        val adjacentEdges = mutable.ListBuffer[Edge]()
        for (edge <- edges) {
          if (edge.source == node) {
            adjacentEdges.addOne(edge)
          }
        }
      }

      adjacencyMap.toMap
    }

    def funA(nodeA: Node, nodeB: Node): Option[(Int, List[Node])] = {
      val mapA = mutable.Map[Node, (Int, List[Node])]()
      mapA(nodeA) = (0, List(nodeA))

      while (mapA.nonEmpty) {
        val currentNode = mapA.minByOption(_._2._1)
        if (currentNode.isEmpty || currentNode.get._1 == nodeB) {
          return Some(mapA(nodeB))
        }
        val varA = currentNode.get._2._1
        val listB = currentNode.get._2._2
        mapA.remove(currentNode.get._1)
        val listEdge = adjacencyList.getOrElse(currentNode.get._1, List.empty)

        for (edge <- listEdge) {
          val nodeC = edge.dest
          val varB = varA + edge.weight
          val listC = listB.appended(nodeC)

          if (varB < mapA.get(nodeC).map(_._1).getOrElse(Int.MaxValue)) {
            mapA(nodeC) = (varB, listC)
          }
        }
      }
      None
    }
  }
}
