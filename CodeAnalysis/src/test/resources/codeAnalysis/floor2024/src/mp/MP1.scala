package floor2024.mp

object MP1:
  /*
   * What is the result of the function call on line 20?
   */
  
  def f(x: List[Int]): Int =
    var y = 0
    x.foreach { z =>
      if z + y < 10 then
        y += z
      else
        y -= z
    }
    y

  def mp1(): Unit =
    val a = List(1, 2, 3, 4, 5)
    println(f(a))
