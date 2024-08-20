package floor2024.fp

import scala.annotation.tailrec

object FP7:
  /*
   * What does funcA do and what is the result of the function
   * call on line 37?
   */

  def funcA = (a: String) => (b: String) => 
    funcD(a, b, 0, 0, funcB(b), Nil)

  def funcB: String => Array[Int] =
    a =>
      @tailrec
      def funcC(b: Int, c: Int, d: Array[Int]): Array[Int] =
        if b == a.length then d
        else if a(b) == a(c) then funcC(b + 1, 0, d.updated(b, c + 1))
        else if c != 0 then funcC(b, d(c - 1), d)
        else funcC(b + 1, 0, d.updated(b, 0))
      funcC(1, 0, Array(a.length))

  @tailrec
  def funcD(a: String, b: String, c: Int, d: Int, e: Array[Int], result: List[Int]): List[Int] =
    if c == a.length then result
    else if a(c) == b(d) then
      if d == b.length - 1 then
        funcD(a, b, c + 1, d + 1, e, result.appended(c - d))
      else
        funcD(a, b, c + 1, d, e, result)
    else if d != 0 then funcD(a, b, c, e(d - 1), e, result)
    else funcD(a, b, c + 1, d, e, result)

  @main def fp7_main(): Unit =
    val varA = funcA("ABABDABACDABABCABAB")("ABA")
    varA match
      case Nil => println("Empty")
      case _ => println(varA)
