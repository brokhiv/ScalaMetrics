package floor2024.mp

object MP2:
  /*
   * What is the functionality of funcA, and what does fundB do?
   */

  def funcA(listA: List[Int]): List[Int] =
    if listA.size <= 1 then
      return listA
    val varA = listA.size / 2
    val listB = listA.slice(0, varA)
    val listC = listA.slice(varA, listA.size)
    funcB(funcA(listB), funcA(listC))

  def funcB(listA: List[Int], listB: List[Int]): List[Int] =
    (listA, listB) match
      case (List(), _) => listB
      case (_, List()) => listA
      case _ if listA.head < listB.head =>
        List(listA.head) ++ funcB(listA.drop(1), listB)
      case _ =>
        List(listB.head) ++ funcB(listA, listB.drop(1))
