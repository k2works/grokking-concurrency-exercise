package concurrency

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import concurrency.ch07.VoteCounter

class VoteCounterSpec extends AnyFunSuite with Matchers:

  test("countVotes should count votes for each candidate"):
    val votes = List("A", "B", "A", "C", "A", "B")
    val result = VoteCounter.countVotes(votes)
    result("A") shouldBe 3
    result("B") shouldBe 2
    result("C") shouldBe 1

  test("countVotesParallel should count votes using fork-join"):
    val votes = List.fill(100)("A") ++ List.fill(50)("B") ++ List.fill(30)("C")
    val result = VoteCounter.countVotesParallel(votes)
    result("A") shouldBe 100
    result("B") shouldBe 50
    result("C") shouldBe 30

  test("countVotesParallel should handle empty list"):
    val result = VoteCounter.countVotesParallel(List.empty)
    result shouldBe empty

  test("mergeResults should combine two vote counts"):
    val a = Map("A" -> 10, "B" -> 5)
    val b = Map("A" -> 5, "C" -> 3)
    val result = VoteCounter.mergeResults(a, b)
    result("A") shouldBe 15
    result("B") shouldBe 5
    result("C") shouldBe 3
