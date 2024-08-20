package floor2024.oop

import scala.collection.mutable

object OOP7 {
  /*
   * What does funcA do and what is the result of the function
   * call on line 60?
   */

  class D {
    def funcA(a: String, b: String): List[Int] = {
      val varA = a.length
      val varB = b.length
      val result = mutable.ListBuffer[Int]()

      val varC = funcB(b)

      var i = 0
      var varE = 0

      while (i < varA) {
        if (b(varE) == a(i)) {
          i += 1
          varE += 1
        }
        if (varE == varB) {
          result.addOne(i - varE)
          varE = varC(varE - 1)
        } else if (i < varA && b(varE) != a(i)) {
          if (varE != 0)
            varE = varC(varE - 1)
          else
            i += 1
        }
      }
      result.toList
    }

    private def funcB(a: String): Array[Int] = {
      val varA = a.length
      val result = new Array[Int](varA)
      var varC = 0
      var varD = 1

      while (varD < varA) {
        if (a(varD) == a(varC)) {
          varC += 1
          result(varD) = varC
          varD += 1
        } else {
          if (varC != 0)
            varC = result(varC - 1)
          else {
            result(varD) = 0
            varD += 1
          }
        }
      }

      result
    }

  }

  @main def oop7(): Unit = {
    val varB = new D().funcA("ABABDABACDABABCABAB", "ABA")
    if (varB.nonEmpty)
      println(varB)
    else
      println("Empty")
  }
}
