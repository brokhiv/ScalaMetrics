package validator

import java.io.File

import codeAnalysis.analyser.metric.MetricProducer
import org.scalatest.funsuite.AnyFunSuite

class UnitSpec(folder: String, metrics: List[MetricProducer]) extends AnyFunSuite {
  test("Akka test") {
    val results = new File(s"data/metricResults/$folder/akka")
    if (!results.exists() || results.list().isEmpty) {
      val validator = new Validator(
        "akka",
        "akka",
        "release-2.8", //FIXedME change branch
        new File("data/projects/akka"),
        results,
        List("bug"),
        metrics
      )
      validator.run()
    } else println("Skipped, metrics are already calculated")
  }

  test("Chisel test") {
    val results = new File(s"data/metricResults/$folder/chisel")

    if (!results.exists() || results.list().isEmpty) {
      val validator = new Validator(
        "chipsalliance",
        "chisel",
        "6.x", //FIXedME head commit is null
        new File("data/projects/chisel"),
        results,
        List("bug"),
        metrics
      )
      validator.run()
    } else println("Skipped, metrics are already calculated")
  }

  test("FS2 test") {
    val results = new File(s"data/metricResults/$folder/fs2")
    if (!results.exists() || results.list().isEmpty) {
      val validator = new Validator(
        "typelevel",
        "fs2",
        "main",
        new File("data/projects/fs2"),
        results,
        List("bug"),
        metrics
      )
      validator.run()
    } else println("Skipped, metrics are already calculated")
  }

  test("Gatling test") {
    val results = new File(s"data/metricResults/$folder/gatling")
    if (!results.exists() || results.list().isEmpty) {
      val validator = new Validator(
        "gatling",
        "gatling",
        "main",
        new File("data/projects/gatling"),
        results,
        List("Type - Bug"),
        metrics
      )
      validator.run()
    } else println("Skipped, metrics are already calculated")
  }

  test("Gitbucket test") {
    val results = new File(s"data/metricResults/$folder/gitbucket")
    if (!results.exists() || results.list().isEmpty) {
      val validator = new Validator(
        "gitbucket",
        "gitbucket",
        "master",
        new File("data/projects/gitbucket"),
        results,
        List("bug"),
        metrics
      )
      validator.run()
    } else println("Skipped, metrics are already calculated")
  }

  test("Http4s test") {
    val results = new File(s"data/metricResults/$folder/http4s")
    if (!results.exists() || results.list().isEmpty) {
      val validator = new Validator(
        "http4s",
        "http4s",
        "series/0.23",//FIXedME change branch
        new File("data/projects/http4s"),
        results,
        List("bug"),
        metrics
      )
      validator.run()
    } else println("Skipped, metrics are already calculated")
  }

  test("Lila test") {
    val results = new File(s"data/metricResults/$folder/lila")
    if (!results.exists() || results.list().isEmpty) {
      val validator = new Validator(
        "lichess-org",
        "lila",
        "volatility-weighted-accuracy", // last PR before Scala 3
        new File("data/projects/lila"),
        results,
        List("bug"),
        metrics
      )
      validator.run()
    } else println("Skipped, metrics are already calculated")
  }

  test("Metals test") {
    val results = new File(s"data/metricResults/$folder/metals")
    if (!results.exists() || results.list().isEmpty) {
      val validator = new Validator(
        "scalameta",
        "metals",
        "main",
        new File("data/projects/metals"),
        results,
        List("bug"),
        metrics
      )
      validator.run()
    } else println("Skipped, metrics are already calculated")
  }

//  test("Quill test") {
//    val validator = new Validator(
//      "getquill",
//      "quill",
//      "master",
//      new File("data/projects/quill"),
//      new File(s"data/metricResults/$folder/quill"),
//      List("bug"),
//      metrics
//    )
//    validator.run()
//  }

//  test("Scio test") {
//    val validator = new Validator(
//      "spotify",
//      "scio",
//      "master",
//      new File("data/projects/scio"),
//      new File(s"data/metricResults/$folder/scio"),
//      List("bug \uD83D\uDC1E"),
//      metrics
//    )
//    validator.run()
//  }

//  test("Shapeless test") {
//    val validator = new Validator(
//      "milessabin",
//      "shapeless",
//      "master",
//      new File("data/projects/shapeless"),
//      new File(s"data/metricResults/$folder/shapeless"),
//      List("Bug"),
//      metrics
//    )
//    validator.run()
//  }

  test("SynapseML test") {
    val results = new File(s"data/metricResults/$folder/SynapseML")
    if (!results.exists() || results.list().isEmpty) {
      val validator = new Validator(
        "microsoft",
        "SynapseML",
        "master",
        new File("data/projects/SynapseML"),
        results,
        List("Bug"),
        metrics
      )
      validator.run()
    } else println("Skipped, metrics are already calculated")
  }

  test("ZIO test") {
    val results = new File(s"data/metricResults/$folder/zio")
    if (!results.exists() || results.list().isEmpty) {
      val validator = new Validator(
        "zio",
        "zio",
        "series/2.x", //FIXedME change branch
        new File("data/projects/zio"),
        results,
        List("bug"),
        metrics
      )
      validator.run()
    } else println("Skipped, metrics are already calculated")
  }
}
