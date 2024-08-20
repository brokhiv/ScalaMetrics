package floor2024.oop

import scala.util.control.Breaks.breakable

object OOP3 {
  /*
   * What is the output of line 29 and 30
   * and what do funcA and funcB do
   */

  def funcA(valA: Int): Boolean = {
    if (valA < 2)
      return false
    for (i <- 2 until valA / 2) {
      if (valA % i == 0) return false
    }
    true
  }

  def funcB(valA: Int, valB: Int): Int = {
    var varA = valA
    var varB = valB
    while (varB != 0) {
      val varC = varB
      varB = varA % varB
      varA = varC
    }
    varA
  }

  @main def oop3(): Unit = {
    println(funcB(12, 16))
    println(funcA(15))
  }
}
