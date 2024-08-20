package floor2024.fp

object FP2 {
  /*
   * What is the functionality of funcA, and what does fundB do?
   */

  def funcA: List[Int] => List[Int] = listA => {
    if (listA.size <= 1) listA
    else {
      val varA = listA.size / 2
      val listB = listA.slice(0, varA)
      val listC = listA.slice(varA, listA.size)
      funcB(funcA(listB), funcA(listC))
    }
  }

  def funcB: (List[Int], List[Int]) => List[Int] = {
    case (Nil, listB) => listB
    case (listA, Nil) => listA
    case (listA@(a :: as), listB@(b :: bs)) => {
      if (a < b) a :: funcB(as, listB)
      else b :: funcB(listA, bs)
    }
  }
}
