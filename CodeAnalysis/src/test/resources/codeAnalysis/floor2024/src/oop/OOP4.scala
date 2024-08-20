package floor2024.oop

object OOP4 {
  /*
   * What does this class represent and what is printed on line 17?
   */

  class A(private val listA: List[Double]) {
    def funcA(a: Double): Double = {
      var varA = 0.0
      for (i <- listA.indices) {
        varA += listA(i) * math.pow(a, i.toDouble)
      }
      varA
    }
  }

  @main def oop4(): Unit = {
    val varA = A(List(3.0, 0.0, -2.0, 1.5))
    println(varA.funcA(2.0))
  }
}
