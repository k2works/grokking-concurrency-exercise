package concurrency.ch07

import scala.concurrent.{Future, Await, ExecutionContext}
import scala.concurrent.duration.Duration

object VoteCounter:
  given ExecutionContext = ExecutionContext.global

  /** Count votes sequentially */
  def countVotes(votes: List[String]): Map[String, Int] =
    votes.groupBy(identity).view.mapValues(_.size).toMap

  /** Merge two vote count results */
  def mergeResults(a: Map[String, Int], b: Map[String, Int]): Map[String, Int] =
    (a.keys ++ b.keys).map { key =>
      key -> (a.getOrElse(key, 0) + b.getOrElse(key, 0))
    }.toMap

  /** Count votes using fork-join pattern */
  def countVotesParallel(votes: List[String]): Map[String, Int] =
    if votes.isEmpty then return Map.empty

    val numCores = Runtime.getRuntime.availableProcessors()
    val chunkSize = math.max(1, votes.size / numCores)
    val chunks = votes.grouped(chunkSize).toList

    val futures = chunks.map { chunk =>
      Future(countVotes(chunk))
    }

    val results = futures.map(f => Await.result(f, Duration.Inf))
    results.reduce(mergeResults)
