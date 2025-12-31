package concurrency

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import concurrency.ch02.PasswordCracker

class PasswordCrackerSpec extends AnyFunSuite with Matchers:

  test("getCombinations should generate all combinations of given length"):
    val combinations = PasswordCracker.getCombinations(2)
    combinations should contain("aa")
    combinations should contain("az")
    combinations should contain("za")
    combinations should contain("zz")
    combinations.length shouldBe 676 // 26 * 26

  test("getCryptoHash should return SHA-256 hash"):
    val hash = PasswordCracker.getCryptoHash("ab")
    hash shouldBe "fb8e20fc2e4c3f248c60c39bd652f3c1347298bb977b8b4d5903b85055620603"

  test("checkPassword should return true for matching hash"):
    val hash = PasswordCracker.getCryptoHash("test")
    PasswordCracker.checkPassword("test", hash) shouldBe true

  test("checkPassword should return false for non-matching hash"):
    val hash = PasswordCracker.getCryptoHash("test")
    PasswordCracker.checkPassword("wrong", hash) shouldBe false

  test("crackPassword should find password for given hash"):
    val password = "ab"
    val hash = PasswordCracker.getCryptoHash(password)
    PasswordCracker.crackPassword(hash, 2) shouldBe Some(password)

  test("crackPassword should return None for non-existent password"):
    val hash = "0000000000000000000000000000000000000000000000000000000000000000"
    PasswordCracker.crackPassword(hash, 2) shouldBe None
