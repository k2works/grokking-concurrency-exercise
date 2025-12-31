package concurrency.ch05

import concurrency.ch02.PasswordCracker
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.{Future, Await, ExecutionContext}
import scala.concurrent.duration.Duration

case class ChunkRange(start: Int, end: Int)

object PasswordCrackerParallel:
  given ExecutionContext = ExecutionContext.global

  /** Divide a range into chunks */
  def getChunks(numChunks: Int, totalSize: Int): List[ChunkRange] =
    val chunkSize = totalSize / numChunks
    val remainder = totalSize % numChunks

    (0 until numChunks).map { i =>
      val start = i * chunkSize + math.min(i, remainder)
      val end = start + chunkSize + (if i < remainder then 1 else 0)
      ChunkRange(start, end)
    }.toList

  /** Crack password using parallel execution */
  def crackPasswordParallel(cryptoHash: String, length: Int): Option[String] =
    val combinations = PasswordCracker.getCombinations(length)
    val numCores = Runtime.getRuntime.availableProcessors()
    val chunks = getChunks(numCores, combinations.length)

    val result = new AtomicReference[Option[String]](None)

    val futures = chunks.map { chunk =>
      Future {
        val subList = combinations.slice(chunk.start, chunk.end)
        subList.find { password =>
          result.get().isEmpty && PasswordCracker.checkPassword(password, cryptoHash)
        } match
          case Some(found) => result.compareAndSet(None, Some(found))
          case None => ()
      }
    }

    futures.foreach(f => Await.ready(f, Duration.Inf))
    result.get()
