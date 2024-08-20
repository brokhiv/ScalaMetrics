package codeAnalysis.floor2024

import codeAnalysis.UnitSpec
import codeAnalysis.analyser.Analyser
import codeAnalysis.analyser.metric.MetricProducer
import codeAnalysis.metrics.paradigmScore._
import codeAnalysis.metrics.baseline._

class BaselineTest extends UnitSpec {

  val subpackages = List("oop", "mp", "fp")

  def print(producer: MetricProducer, subpackage: String): Unit = {
    val metrics = new Analyser(s"${resources}floor2024/src/$subpackage", List(producer), true).analyse()
    metrics.foreach(println)
  }

  def printAll(producers: List[MetricProducer]): Unit = {
    for (pack <- subpackages) {
      for (prod <- producers) print(prod, pack)
    }
  }

  test("Baseline test") {
    val baselines = List(
      LinesOfCode,
      DepthOfInheritance,
      NumberOfChildren,
      LackOfCohesionInMethods,
      ResponseForClass,
      CyclomaticComplexity,
      WeightedMethodCount,
      CouplingBetweenObjects,
      PatternSize,
      OutDegree,
      DepthOfNesting,
    )
    printAll(baselines)
  }

  test("Paradigm Score test") {
    val paradigmScores = List(
      ParadigmScoreBool,
      ParadigmScoreCount,
      ParadigmScoreFraction,
      ParadigmScoreLandkroon,
    )
    printAll(paradigmScores)
  }
}
