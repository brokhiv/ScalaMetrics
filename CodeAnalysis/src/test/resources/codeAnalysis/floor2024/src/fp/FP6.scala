package floor2024.fp

object FP6 {
  /*
   * This type represents a matrix.
   * The result on line 39 is [2,4,8] but this is not
   * entirely correct, why and what is missing?
   */

  type C = List[List[Int]]

  def funcA: C => C = {
    case Nil => Nil
    case Nil :: xss => Nil
    case xs :: xss => {
      def go: (List[Int], C) => C = {
        case (Nil, _) => Nil
        case (x :: xs, xss) => (x :: xss.map(_.head)) :: go(xs, xss.map(_.tail))
      }

      go(xs, xss)
    }
  }

  def funcB = (valA: C) => (lambA: C => (List[Int] => List[Int]) => C) => (lambB: Int => Boolean) => lambA(valA)(funcC(_)(lambB))

  def funcC = (a: List[Int]) => (lambA: Int => Boolean) => a.filter(lambA(_))

  @main def fp6_main(): Unit = {
    val varA = List(
      List(1, 2, 3),
      List(4, 5, 6),
      List(7, 8, 9),
    )
    val lambA: C => (List[Int] => List[Int]) => C = a => lambB => a.map(lambB(_))
    val result = funcB(varA)(lambA)(_ % 2 == 0)
    funcA(result).foreach(println)
  }
}
