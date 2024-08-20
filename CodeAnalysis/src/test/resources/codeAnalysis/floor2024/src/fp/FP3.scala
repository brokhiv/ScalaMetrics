package floor2024.fp

import scala.annotation.tailrec

object FP3 {
  /*
   * What is the output of line 20 and 21
   * and what do funcA and funcB do
   */

  def funcA = (a: Int) => a > 1 && (2 until a).forall(a % _ != 0)

  @tailrec
  def funcB(a: Int, b: Int): Int = b match {
    case 0 => a
    case _ => funcB(b, a % b)
  }

  @main def fp3(): Unit = {
    println(funcB(12, 16))
    println(funcA(15))
  }
}
