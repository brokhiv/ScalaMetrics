package floor2024.mp

object MP6:
  /*
   * This class has a matrix as its variable.
   * The result on line 40 is [2,4,8] but this is not
   * entirely correct, why and what is missing?
   */
  
  class C(val valA: List[List[Int]]):
    def funcA(): C =
      val valB = valA.size
      val valC = if valB > 0 then valA(0).size else 0
      C(for a <- (0 until valC).toList yield {
        for b <- (0 until valB).toList yield valA(b)(a)
      })
  
    def funcB(
      lambA: (List[List[Int]], List[Int] => List[Int]) => List[List[Int]],
      lambB: Int => Boolean
    ): C =
      C(lambA(valA, a => funcC(a, lambB)))
    
    private def funcC(a: List[Int],
                      lambA: Int => Boolean): List[Int] =
      a.filter(lambA(_))

  end C
  
  @main def mp6(): Unit =
    val varA = List(
      List(1, 2, 3),
      List(4, 5, 6),
      List(7, 8, 9)
    )
    val varB = C(varA)
    val lambA: (List[List[Int]], (List[Int] => List[Int])) => List[List[Int]] = 
      (a, lambB) => a.map { b => lambB(b) }
    val result = varB.funcB(lambA, a => a % 2 == 0)
    result.funcA().valA.foreach { a => println(a) }
