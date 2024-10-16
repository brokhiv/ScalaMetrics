package codeAnalysis.metrics

import codeAnalysis.UnitSpec
import codeAnalysis.analyser.Analyser
import codeAnalysis.analyser.metric.MetricProducer
import codeAnalysis.metrics.baseline._
import codeAnalysis.metrics.multiparadigm.constructs._
import codeAnalysis.metrics.multiparadigm.zuilhof._
import codeAnalysis.metrics.paradigmScore._

class MetricTest extends UnitSpec {
  def print(producer: MetricProducer): Unit = {
    val metrics = new Analyser(resources + "metrics", List(producer), true).analyse()
    metrics.foreach(println)
  }

  test("Paradigm Score Bool test") {
    print(ParadigmScoreBool)
  }

  test("Paradigm Score Count test") {
    print(ParadigmScoreCount)
  }

  test("Paradigm Score Fraction test") {
    print(ParadigmScoreFraction)
  }

  test("Paradigm Score Landkroon test") {
    print(ParadigmScoreLandkroon)
  }

  test("Lines of Code test") {
    print(LinesOfCode)
  }

  test("Depth of Inheritance test") {
    print(DepthOfInheritance)
  }

  test("Number of Children test") {
    print(NumberOfChildren)
  }

  test("Lack Of Cohesion In Methods test") {
    print(LackOfCohesionInMethods)
  }

  test("Response for Class test") {
    print(ResponseForClass)
  }

  test("Cyclomatic Complexity test") {
    print(CyclomaticComplexity)
  }

  test("Weighted Methoc Count test") {
    print(WeightedMethodCount)
  }

  test("Coupling Between Objects test") {
    print(CouplingBetweenObjects)
  }

  test("Pattern Size test") {
    print(PatternSize)
  }

  test("Out Degree test") {
    print(OutDegree)
  }

  test("Depth of Nesting test") {
    print(DepthOfNesting)
  }

  test("Number of Lambda Functions test") {
    print(NumberOfLambdaFunctions)
  }

  test("Source Lines of Lambda test") {
    print(SourceLinesOfLambda)
  }

  test("Lambda Score test") {
    print(LambdaScore)
  }

  test("Number of Lambda Functions Using Variables test") {
    print(LambdaFunctionsUsingVariables)
  }

  test("Number of Lambda Functions With Side-Effects test") {
    print(LambdaFunctionsWithSideEffects)
  }

  test("Number of overriding pattern variables test") {
    print(OverridingPatternVariables)
  }

  test("Number of implicits test") {
    print(NumberOfImplicits)
  }

  test("Variable types test") {
    print(VariableTypes)
  }

  test("Usage of Null test") {
    print(UsageOfNull)
  }

  test("Number of Returns test") {
    print(NumberOfReturns)
  }
}
