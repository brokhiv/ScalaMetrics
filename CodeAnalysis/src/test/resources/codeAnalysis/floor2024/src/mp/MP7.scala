package floor2024.mp

import scala.collection.mutable

object MP7:
  /*
   * What does funcA do and what is the result of the function
   * call on line 44?
   */

  class E:
    def funcA(a: String, b: String): List[Int] =
      val varA = funcB(b)
      funcD(a, b, 0, 0, varA, mutable.ListBuffer())

    private def funcB(a: String): Array[Int] =
      def funcC(b: Int, c: Int, d: Array[Int]): Array[Int] =
        if b == a.length then d
        else if a(b) == a(c) then
          d(b) = c + 1
          funcC(b + 1, 0, d)
        else if c != 0 then funcC(b, d(c - 1), d)
        else
          d(b) = 0
          funcC(b + 1, 0, d)

      funcC(1, 0, Array(a.length))

    private def funcD(a: String, b: String, c: Int, d: Int,
                      e: Array[Int], result: mutable.ListBuffer[Int]): List[Int] =
      if c == a.length then result.toList
      else if a(c) == b(d) then
        if d == b.length - 1 then
          result.addOne(c - d)
          funcD(a, b, c + 1, e(d - 1), e, result)
        else
          funcD(a, b, c + 1, d + 1, e, result)
      else if d != 0 then funcD(a, b, c, e(d - 1), e, result)
      else funcD(a, b, c + 1, d, e, result)

  end E

  @main def mp7_main(): Unit =
    val varA = E().funcA("ABABDABACDABABCABAB", "ABA")
    if varA.nonEmpty then
      println(varA)
    else
      println("Empty")
