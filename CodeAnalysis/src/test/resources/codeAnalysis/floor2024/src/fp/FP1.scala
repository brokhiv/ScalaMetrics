package floor2024.fp

object FP1:
  /*
   * What is the result of the function call on line 15?
   */
  
  def g: List[Int] => Int =
    _.foldLeft(0)( (y, z) =>
      if y + z < 10 then y + z else y - z
    )

  @main def fp1(): Unit =
    val a = List(1, 2, 3, 4, 5)
    println(g(a))
