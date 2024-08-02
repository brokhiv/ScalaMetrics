package codeAnalysis.util

import scala.reflect.internal.util.SourceFile

object Extensions {

  /**
   * Logarithm base 2.
   * @param x input to the log2 function
   * @return log base 2 of x
   */
  def log2(x: Double): Double = math.log(x)/math.log(2)

  /**
   * Logarithm base 2 multiplied with the parameter, useful for entropy calculations.
   * @param n input to the function, may be 0
   * @return n * log2(n), or 0 if n == 0
   */
  def nlogn(n: Double): Double = if (n == 0) 0 else n * log2(n)

  implicit class DoubleExtension(double: Double) {
    /**
     * Safe division, returning 0 when the denominator is 0.
     * @param other denominator of the fraction
     * @return double / other, or 0 if other == 0
     */
    def \(other: Double): Double = if (other != 0.0) double / other else 0.0
  }

  implicit class BooleanExtension(boolean: Boolean) {
    def toInt: Int = if (boolean) 1 else 0
  }

  implicit class NumericTupleExtension[A: Numeric, B: Numeric](t: (A, B)) {

    import Numeric.Implicits._

    def +(p: (A, B)): (A, B) = (p._1 + t._1, p._2 + t._2)
  }

  implicit class ListExtension[A](iterable: List[A]) {
    def zipWith[B, C](other: List[B])(f: (A, B) => C): List[C] = iterable.zip(other).map(f.tupled)
  }

  implicit class SourceFileExtension(sourceFile: SourceFile) {
    def text = new String(sourceFile.content)
  }
}
