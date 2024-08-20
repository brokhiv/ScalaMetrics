package floor2024.fp

import scala.math.pow

object FP4:
  /*
   * What does this type represent and what is printed on line 21?
   */

  type A = List[Double]

  def funcA = (listA: A) => (a: Double) => 
    listA
      .lazyZip(LazyList.from(0))
      .map((c, i) => c * pow(a, (i.toDouble)))
      .sum
  
  @main def fp4(): Unit =
    val varA= List(3.0, 0.0, -2.0, 1.5)
    println(funcA(varA)(2.0))
