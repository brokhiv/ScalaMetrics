package floor2024.mp
import scala.collection.mutable

object MP5:
  /*
   * You have been given a case class Edge and Node,
   * and within the class Graph there are 2 to be identified functions.
   * What does funA do and what does it return?
   */

  case class Edge(source: Node, dest: Node, weight: Int)
  case class Node(name: String)
  class Graph(val nodes: List[Node], val edges: List[Edge]):
    private val adjacencyList: Map[Node, List[Edge]] =
      nodes
        .map(node => (node, edges.filter(_.source == node)))
        .toMap

    def funcA(nodeA: Node, nodeB: Node): Option[(Int, List[Node])] =
      val (listA, mapA) = nodes.foldLeft(
        (List[Node](),
          Map((nodeA, (0, List(nodeA)))))
      ) { case ((listB, mapB: Map[Node, (Int, List[Node])]), _) =>
        if listB.isEmpty || listB.last != nodeB then
          val currentNode: Option[(Node, (Int, List[Node]))] = mapB
            .view.filterKeys(!listB.contains(_))
            .minByOption(_._2._1)
          val pairBnodeB = mapB.get(nodeB)
          if pairBnodeB.isDefined &&
            (currentNode.isEmpty ||
              currentNode.get._1 == nodeB) then
            (pairBnodeB.get._2, mapB)
          else
            val varA = mapB.get(currentNode.map(_._1).orNull).map(_._1).get
            val listC = mapB.get(currentNode.map(_._1).orNull).map(_._2).getOrElse(List())
            val listD = adjacencyList.getOrElse(currentNode.map(_._1).orNull, List())
            val mapB_ = funcB(listD, nodeA, mapB, varA, listC)
            val listE = if !currentNode.map(_._1).contains(nodeA) then
              listB :+ currentNode.get._1 else
              List(currentNode.get._1)
            (listE, mapB_)
        else
          (listB, mapB)
      }
      if listA.nonEmpty && listA.last == nodeB then
        mapA.get(nodeB) else None
    
    private def funcB(
     listEdge: List[Edge],
     nodeA: Node,
     mapA: Map[Node, (Int, List[Node])],
     a: Int,
     listNode: List[Node]
   ): Map[Node, (Int, List[Node])] =
      listEdge.foldLeft(mutable.Map.from(mapA)) { (mapB, edge) =>
        val varA = a + edge.weight
        val (varB, _) = mapB.getOrElse(edge.dest, (Int.MaxValue, List(nodeA)))
        if varA < varB then 
          mapB.put(edge.dest, (varA, listNode :+ edge.dest))
        mapB
      }.toMap
