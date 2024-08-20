package floor2024.mp

object MP3 {
  /*
   * What is the output of line 16 and 17
   * and what do funcA and funcB do
   */

  def funcA(a: Int): Boolean = a > 1 && (2 until a).forall(a % _ != 0)

  def funcB(a: Int, b: Int): Int = if (b == 0) a else funcB(b, a % b)

  @main def mp3(): Unit = {
    println(funcB(12, 16))
    println(funcA(15))
  }
}
