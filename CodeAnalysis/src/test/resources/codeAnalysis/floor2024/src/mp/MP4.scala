package floor2024.mp
import scala.math.pow

object MP4:
  /*
   * What does this class represent and what is printed on line 17?
   */

  class A(private val listA: List[Double]):
    def funcA(a: Double): Double =
      listA
        .zipWithIndex
        .map((c, i) => c * pow(a, (i.toDouble)))
        .sum
    
  end A
  
  @main def mp4(): Unit =
    val varA = A(List(3.0, 0.0, -2.0, 1.5))
    println(varA.funcA(2.0))
