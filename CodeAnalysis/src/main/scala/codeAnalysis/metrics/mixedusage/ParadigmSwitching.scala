package codeAnalysis.metrics.mixedusage

import codeAnalysis.analyser.Compiler
import codeAnalysis.analyser.metric.{MethodMetric, Metric, MetricProducer, MetricResult, ObjectMetric}
import codeAnalysis.util.Extensions.IntExtension

import scala.reflect.api.Position
import scala.reflect.internal.Flags._

object ParadigmSwitching extends MetricProducer {

  /**
   * Create a new metric instance for the specified compiler.
   *
   * @param compiler the specified compiler
   * @return the created metric instance
   */
  override def apply(compiler: Compiler): Metric = new ParadigmSwitching(compiler)

  object Paradigm extends Enumeration {
    type Paradigm = Value
    val Imperative, Functional, Neutral, Inter = Value

    implicit class ParadigmComparison(paradigm: Value) {
      def |->(next: Value): Value = (paradigm, next) match {
        case (Imperative, Inter) => Functional
        case (Functional, Inter) => Imperative
        case (_, Inter) => paradigm
        case (_, Neutral) => paradigm
        case _ => next
      }

      def switch(next: Value) = (paradigm, next) match {
        case (Neutral, _) => 0
        case (_, Neutral) => 0
        case _ => if (paradigm == next) 0 else 1
      }
    }
  }
}

class ParadigmSwitching(override val compiler: Compiler) extends ObjectMetric with MethodMetric {
  import global.TreeExtensions
  import ParadigmSwitching.Paradigm

  def isRecursive(defDef: ParadigmSwitching.this.global.DefDef): Boolean = defDef.existsTraverse {
    case apply: global.Apply => defDef.symbol == apply.symbol
  }

  def classifyNode(tree: global.ImplDef): Paradigm.Value = tree match {
    case classDef: global.ClassDef => // Could be a class, trait or case class
      if (classDef.mods.hasFlag(CASE | LAZY | SEALED)) Paradigm.Functional // ADT or lazy
      else if (classDef.mods hasFlag IMPLICIT) Paradigm.Inter // Implicit class
      else if (classDef.mods.hasFlag(ABSTRACT | PRIVATE | PROTECTED)) Paradigm.Imperative // Inheritance-based or encapsulated
      else Paradigm.Neutral // Normal class/trait
    case moduleDef: global.ModuleDef =>
      if (moduleDef.mods.hasFlag(CASE | LAZY)) Paradigm.Functional // ADT component or lazy object
      else Paradigm.Neutral // An object is considered Neutral
    case _ => throw new IllegalArgumentException(s"Unrecognized instance of ImplDef: $tree is ${tree.getClass}")
  }

  def classifyNode(tree: global.ValOrDefDef): Paradigm.Value = tree match {
    case valDef: global.ValDef => // Could be a val or var, or a parameter
      if (valDef.mods hasFlag IMPLICIT) Paradigm.Inter // Implicit value/parameter
      else if (valDef.isVar) Paradigm.Imperative // Mutable variable
      else if (valDef.isLazy || valDef.isFunction) Paradigm.Functional // Lazy or function value
      else Paradigm.Neutral // Regular parameter/value
    case defDef: global.DefDef => // Any method (def)
      if (defDef.vparamss.length > 1 || isRecursive(defDef)) Paradigm.Functional // Curried or recursive
      else Paradigm.Neutral // Regular method
    case _ => throw new IllegalArgumentException(s"Unrecognized instance of ValOrDefDef: $tree is ${tree.getClass}")
  }

  def classifyNode(tree: global.Tree): Paradigm.Value = tree match {
    case _: global.Match => Paradigm.Functional // pattern match or destruct
    case labelDef: global.LabelDef if labelDef.name containsName "while" => Paradigm.Imperative // while loop
    case term@(_: global.TermTree | _: global.SymTree)
      if term.isFunction && (term.isVar || tree.isUnit) => Paradigm.Inter // mutable/side effect function
    case term@(_: global.TermTree | _: global.SymTree)
      if term.isFunction => Paradigm.Functional // pure function
    case term@(_: global.TermTree | _: global.SymTree)
      if term.isVar || tree.isUnit => Paradigm.Imperative // mutable or side effect value
    case _: global.Assign => Paradigm.Imperative // assign mutable variable, may double-count mutable variable usage
    case apply: global.Apply
      if apply.isUnit && (apply.args.exists(_.isFunction) || apply.isFunction) => Paradigm.Inter // side effect function
    case apply: global.Apply
      if apply.args.exists(_.isFunction) || apply.isFunction => Paradigm.Functional // (higher-order) function application
    case apply: global.Apply
      if apply.isUnit => Paradigm.Imperative // side effect call
    case tree: global.Select
      if tree.isVar => Paradigm.Imperative // mutable variable access, may double-count
    case function: global.Function
      if function.body.isUnit => Paradigm.Inter // function with side effect
    case _: global.Function => Paradigm.Functional // Function literal
    case _ => Paradigm.Neutral
  }

  def collectConstructs(tree: global.ImplDef): List[(Position, Paradigm.Value)] = tree.collectTraverse {
    case node: global.ImplDef => (node.pos, classifyNode(node))
    case node: global.ValOrDefDef => (node.pos, classifyNode(node))
    case node => (node.pos, classifyNode(node))
  }

  def collectConstructs(tree: global.DefDef): List[(Position, Paradigm.Value)] = tree.collectTraverse {
    case node: global.ValOrDefDef => (node.pos, classifyNode(node))
    case node: global.ImplDef => (node.pos, classifyNode(node))
    case node => (node.pos, classifyNode(node))
  }

  /**
   * Runs this metric on the specified tree
   *
   * @param tree the specified tree
   * @return the list of metric results
   */
  override def run(tree: global.ImplDef): List[MetricResult] = {
    val tags = collectConstructs(tree).sortBy(_._1.pointOrElse(-1)) // We can assume that we are in one source file, so the ordering is purely based on the offset
    println(tags.map { case (p, v) => (p.lineContent, v) }.mkString("\n")) // Debug print

    process(tags)
  }

  override def run(tree: global.DefDef): List[MetricResult] = {
    val tags = collectConstructs(tree).sortBy(_._1.point)
    println(tags.map { case (p, v) => (p.line, v) }.mkString("\n")) // Debug print

    process(tags)
  }

  private def process(tags: List[(Position, ParadigmSwitching.Paradigm.Value)]) = {
    val switchCount = tags
      .map(_._2)
      .foldLeft((0, Paradigm.Neutral)) {
        case ((count, current), next) => (count + current.switch(next), current |-> next)
      }
      ._1

    val centers = tags
      .map(_._2)
      .zipWithIndex
      .filterNot(_._1 == Paradigm.Neutral)
      .foldLeft(List[(ParadigmSwitching.Paradigm.Value, Int)]()) {
        case (Nil, (par, idx)) => (par, idx) :: Nil
        case (ps, (par, idx)) if ps.head._1.switch(par) == 1 => (par, idx) :: ps
        case (ps, _) => ps
      }
      .map(_._2)
      .reverse

    val stretches = centers
      .map { idx =>
        val (before, (pos, par) :: after) = tags.splitAt(idx)
        val begin = before.reverse.findLast { case (_, prev) => prev.switch(par) == 0 }.map(_._1).getOrElse(pos)
        val end = after.findLast { case (_, next) => par.switch(next) == 0 }.map(_._1).getOrElse(pos)
        end.line - begin.line
      }

    List(
      MetricResult("Switch count", switchCount),
      MetricResult("Max stretch", stretches.maxOption.getOrElse(0).doubleValue),
      MetricResult("Min stretch", stretches.minOption.getOrElse(0).doubleValue),
      MetricResult("Average stretch", stretches.sum \! stretches.length),
    )
  }
}
