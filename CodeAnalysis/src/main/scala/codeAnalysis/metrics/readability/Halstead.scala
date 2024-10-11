package codeAnalysis.metrics.readability

import codeAnalysis.analyser.{Compiler, metric}
import codeAnalysis.analyser.metric.{FileMetric, MethodMetric, Metric, MetricProducer, MetricResult, ObjectMetric}
import codeAnalysis.util.Extensions.log2

import scala.reflect.runtime.universe.ValDef




object Halstead extends MetricProducer {
  object HalsteadType extends Enumeration {
    type HalsteadType = Value
    val Operator, Operand, Neither = Value
  }

  /**
   * Create a new metric instance for the specified compiler.
   *
   * @param compiler the specified compiler
   * @return the created metric instance
   */
  override def apply(compiler: Compiler): Metric = new Halstead(compiler)
}
class Halstead(override val compiler: Compiler) extends FileMetric with ObjectMetric with MethodMetric {
  import global.TreeExtensions

  /**
   * Wraps a terminal in Left if it is an operator, or in Right if it is an operand.
   * In case a terminal is neither or in case of a non-terminal, the function is not defined.
   * @param tree the tree to be evaluated
   * @return Left(symbol) if tree is an operator, Right(symbol) if tree is an operand, no return otherwise
   *         symbol represents the name of the operator/operand
   */
  private def collectTokens: PartialFunction[global.Tree, Either[String, String]] = {
//    case ValDef(mods, name, _, _) => List(Right(name.toString), Left("=")) // type tree and right-hand side will be traversed
  ???}

  def halstead(tree: global.Tree): List[MetricResult] = {
    /*
    Operator: function invocation (+1 for ()), punctuation, keywords, operator symbols
    Operand: variable (declared/used/assigned), literal
     */
    val (operators, operands) = tree.collect(collectTokens).partitionMap(x => x)

    val distinctOperators = operators.distinct.size
    val distinctOperands = operands.distinct.size
    val totalOperators = operators.size
    val totalOperands = operands.size

    // Compute derived metrics
    val vocabulary = distinctOperands + distinctOperators
    val length = totalOperands + totalOperators
    val volume = length * log2(vocabulary)
    val difficulty = distinctOperators / 2 * totalOperands / totalOperators
    val effort = volume * difficulty
    val time = effort / 18
    val bugsA = math.pow(effort, 2/3) / 3000
    val bugsB = volume / 3000

    List(
      MetricResult("Halstead Volume", volume),
      MetricResult("Halstead Difficulty", difficulty),
      MetricResult("Halstead Effort", effort),
      MetricResult("Halstead Time", time),
      MetricResult("Halstead Estimated Bugs (A)", bugsA),
      MetricResult("Halstead Estimated Bugs (B)", bugsB),
    )
  }

  /**
   * Runs this metric on the specified tree
   *
   * @param tree the specified tree
   * @return the list of metric results
   */
  override def run(tree: global.PackageDef): List[MetricResult] = halstead(tree)

  /**
   * Runs this metric on the specified tree
   *
   * @param tree the specified tree
   * @return the list of metric results
   */
  override def run(tree: global.ImplDef): List[MetricResult] = halstead(tree)

  /**
   * Runs this metric on the specified tree
   *
   * @param tree the specified tree
   * @return the list of metric results
   */
  override def run(tree: global.DefDef): List[MetricResult] = halstead(tree)

}
