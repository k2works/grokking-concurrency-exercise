package concurrency

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import concurrency.ch07.Pipeline

class PipelineSpec extends AnyFunSuite with Matchers:

  test("Pipeline should process data through stages"):
    val pipeline = Pipeline[Int]()
      .addStage("double", (x: Int) => x * 2)
      .addStage("addOne", (x: Int) => x + 1)

    val results = pipeline.process(List(1, 2, 3))
    results shouldBe List(3, 5, 7) // (1*2)+1, (2*2)+1, (3*2)+1

  test("Pipeline should handle single stage"):
    val pipeline = Pipeline[String]()
      .addStage("uppercase", (s: String) => s.toUpperCase)

    val results = pipeline.process(List("hello", "world"))
    results shouldBe List("HELLO", "WORLD")

  test("Pipeline should handle type conversions"):
    val pipeline = Pipeline[Int]()
      .addStage("toString", (x: Int) => x.toString)
      .addStage("length", (s: String) => s.length)

    val results = pipeline.process(List(1, 10, 100))
    results shouldBe List(1, 2, 3)
