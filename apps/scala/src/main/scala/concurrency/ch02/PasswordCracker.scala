package concurrency.ch02

import java.security.MessageDigest
import scala.annotation.tailrec

object PasswordCracker:
  private val Letters: String = "abcdefghijklmnopqrstuvwxyz"

  /** Generate all combinations of lowercase letters of the given length */
  def getCombinations(length: Int): List[String] =
    @tailrec
    def generate(current: List[String], remaining: Int): List[String] =
      if remaining == 0 then current
      else
        val next = for
          prefix <- current
          letter <- Letters
        yield prefix + letter
        generate(next, remaining - 1)

    generate(List(""), length)

  /** Calculate SHA-256 hash of the given password */
  def getCryptoHash(password: String): String =
    val digest = MessageDigest.getInstance("SHA-256")
    val bytes = digest.digest(password.getBytes("UTF-8"))
    bytes.map(b => f"$b%02x").mkString

  /** Check if the password matches the given hash */
  def checkPassword(password: String, cryptoHash: String): Boolean =
    getCryptoHash(password) == cryptoHash

  /** Crack password by brute force */
  def crackPassword(cryptoHash: String, length: Int): Option[String] =
    getCombinations(length).find(checkPassword(_, cryptoHash))
