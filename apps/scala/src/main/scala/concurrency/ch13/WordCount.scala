package concurrency.ch13

import scala.collection.parallel.CollectionConverters.*

object WordCount:

  /** Map: Convert text to (word, 1) pairs */
  def map(text: String): List[(String, Int)] =
    text.toLowerCase
      .split("\\s+")
      .filter(_.nonEmpty)
      .map(word => (word, 1))
      .toList

  /** Reduce: Aggregate word counts */
  def reduce(pairs: List[(String, Int)]): Map[String, Int] =
    pairs.groupBy(_._1).view.mapValues(_.map(_._2).sum).toMap

  /** MapReduce: Count words in multiple texts using parallel collections */
  def countWords(texts: List[String]): Map[String, Int] =
    // Map phase (parallel)
    val mapped = texts.par.flatMap(map).toList

    // Reduce phase
    reduce(mapped)
