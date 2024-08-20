package floor2024.oop

object OOP1 {
  /*
   * What is the result of the function call on line 19?
   */

  def g(x: List[Int]): Int = {
    var y = 0
    for (z <- x) {
      if (z + y < 10)
        y += z
      else
        y -= z
    }
    y
  }

  @main def oop1(): Unit = {
    val a = List(1, 2, 3, 4, 5)
    println(g(a))
  }
}
