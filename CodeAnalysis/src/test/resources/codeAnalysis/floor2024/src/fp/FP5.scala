package floor2024.fp

import scala.annotation.tailrec

object FP5 {
  /*
   * You have been given a case class Edge, Node and Graph,
   * and there are 4 to be identified functions.
   * What does funA do and what does it return?
   */

  case class Edge(source: Node, dest: Node, weight: Int)

  case class Node(name: String)

  case class Graph(nodes: List[Node], edges: List[Edge]) {
    lazy val adjacencyList: Map[Node, List[Edge]] = {
      nodes
        .map(node => (node, edges.filter(_.source == node)))
        .toMap
    }
  }

  def funcC: Node => Int => ((Map[Node, Int], Map[Node, Node]), Edge) => (Map[Node, Int], Map[Node, Node]) = nodeA => a => {
    case ((mapA, mapNode), Edge(_, dest, weight)) => {
      val varA = mapA.getOrElse(dest, Int.MaxValue)
      val varB = a + weight
      if (varB < varA) (mapA.updated(dest, varB), mapNode.updated(dest, nodeA))
      else (mapA, mapNode)
    }
  }

  def funcD: Node => Node => Map[Node, Node] => List[Node] = nodeA => nodeB => mapNode => {
    if (nodeA == nodeB) List(nodeA)
    else mapNode.get(nodeB) match {
      case Some(node) => nodeB :: funcD(nodeA)(node)(mapNode)
      case None => Nil
    }
  }

  def funcA: Graph => Node => Node => Option[(Int, List[Node])] = graph => nodeA => nodeB => {
    @tailrec
    def funcB(mapA: Map[Node, Int], mapNode: Map[Node, Node], adjacency: Map[Node, List[Edge]]): (Map[Node, Int], Map[Node, Node]) = {
      if (mapA.isEmpty) (mapA, mapNode)
      else {
        val (node, a) = mapA.minBy(_._2)
        val listEdge = adjacency.getOrElse(node, Nil)
        val (mapB, mapNode1) = listEdge.foldLeft((mapA.removed(node), mapNode))(funcC(node)(a))
        funcB(mapB, mapNode1, adjacency)
      }
    }

    val adjacency = graph.adjacencyList
    val (mapA, mapNode) = funcB(Map((nodeA, 0)), Map.empty, adjacency)
    mapA.get(nodeB) match {
      case Some(a) => Some((a, funcD(nodeA)(nodeB)(mapNode).reverse))
      case None => None
    }
  }
}
