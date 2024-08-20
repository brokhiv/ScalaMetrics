package floor2024.oop

import scala.collection.mutable

object OOP6 {
  /*
   * This class has a matrix as its variable.
   * The result on line 52 is [2,4,8] but this is not
   * entirely correct, why and what is missing?
   */

  class C(val valA: List[List[Int]]) {
    def funcA: C = {
      if (valA.isEmpty) return C(List())

      val varA = valA.size
      val varB = if (varA > 0) valA(0).size else 0

      val listA = mutable.ListBuffer[mutable.ListBuffer[Int]]()
      for (i <- 0 until varB) {
        val listB = mutable.ListBuffer[Int]()
        for (j <- 0 until varA) {
          listB.addOne(valA(j)(i))
        }
        listA.addOne(listB)
      }

      C(listA.map(_.toList).toList)
    }

    def funcB: C = {
      val listA = mutable.ListBuffer[List[Int]]()
      for (varA <- valA) {
        val listB = mutable.ListBuffer[Int]()
        for (varB <- varA) {
          if (funcC(varB)) {
            listB.addOne(varB)
          }
        }
        listA.addOne(listB.toList)
      }
      C(listA.toList)
    }

    private def funcC(varA: Int): Boolean = varA % 2 == 0
  }

  @main def oop6_main(): Unit = {
    val varA = List(
      List(1, 2, 3),
      List(4, 5, 6),
      List(7, 8, 9),
    )
    val varB = C(varA)
    val result = varB.funcB
    result.funcA.valA.foreach(println)
  }
}
