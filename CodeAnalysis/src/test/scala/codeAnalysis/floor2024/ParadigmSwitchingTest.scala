package codeAnalysis.floor2024

import codeAnalysis.UnitSpec
import codeAnalysis.analyser.Analyser
import codeAnalysis.analyser.metric.MetricProducer
import codeAnalysis.metrics.mixedusage.ParadigmSwitching

class ParadigmSwitchingTest extends UnitSpec {
  val subpackages = List("oop", "mp", "fp")

  def print(producer: MetricProducer, subpackage: String): Unit = {
    val metrics = new Analyser(s"${resources}floor2024/src/$subpackage", List(producer), true).analyse()
    metrics.foreach(println)
  }

  test("Paradigm Switching test") {
    subpackages.foreach(print(ParadigmSwitching, _))
  }
}
