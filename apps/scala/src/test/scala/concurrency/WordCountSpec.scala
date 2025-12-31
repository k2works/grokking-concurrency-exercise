package concurrency

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import concurrency.ch13.WordCount

class WordCountSpec extends AnyFunSuite with Matchers:

  test("map should convert text to word-count pairs"):
    val result = WordCount.map("hello world hello")
    result should contain(("hello", 1))
    result should contain(("world", 1))
    result.count(_._1 == "hello") shouldBe 2

  test("reduce should aggregate word counts"):
    val pairs = List(("hello", 1), ("world", 1), ("hello", 1))
    val result = WordCount.reduce(pairs)
    result("hello") shouldBe 2
    result("world") shouldBe 1

  test("countWords should perform MapReduce"):
    val texts = List(
      "hello world",
      "hello scala",
      "world of scala"
    )
    val result = WordCount.countWords(texts)
    result("hello") shouldBe 2
    result("world") shouldBe 2
    result("scala") shouldBe 2
    result("of") shouldBe 1
