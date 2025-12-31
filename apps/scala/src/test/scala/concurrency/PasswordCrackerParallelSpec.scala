package concurrency

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import concurrency.ch05.PasswordCrackerParallel
import concurrency.ch02.PasswordCracker

class PasswordCrackerParallelSpec extends AnyFunSuite with Matchers:

  test("getChunks should divide range into equal parts"):
    val chunks = PasswordCrackerParallel.getChunks(4, 100)
    chunks.length shouldBe 4
    chunks.head.start shouldBe 0
    chunks.last.end shouldBe 100

  test("getChunks should handle uneven division"):
    val chunks = PasswordCrackerParallel.getChunks(3, 10)
    chunks.length shouldBe 3
    // Verify all elements are covered
    val covered = chunks.flatMap(c => c.start until c.end).toSet
    covered shouldBe (0 until 10).toSet

  test("crackPasswordParallel should find password"):
    val password = "ab"
    val hash = PasswordCracker.getCryptoHash(password)
    PasswordCrackerParallel.crackPasswordParallel(hash, 2) shouldBe Some(password)

  test("crackPasswordParallel should return None for non-existent password"):
    val hash = "0000000000000000000000000000000000000000000000000000000000000000"
    PasswordCrackerParallel.crackPasswordParallel(hash, 2) shouldBe None

  test("crackPasswordParallel should use multiple threads"):
    val password = "zz"
    val hash = PasswordCracker.getCryptoHash(password)
    // This should be faster with parallel execution
    val result = PasswordCrackerParallel.crackPasswordParallel(hash, 2)
    result shouldBe Some(password)
