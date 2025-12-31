package concurrency.ch04

object ThreadBasics:

  /** Create a worker thread with the given name and task */
  def createWorker(name: String, task: () => Unit): Thread =
    val thread = new Thread(() => task())
    thread.setName(name)
    thread

  /** Create multiple worker threads */
  def createMultipleWorkers(count: Int, task: Int => Unit): List[Thread] =
    (0 until count).map { i =>
      val thread = new Thread(() => task(i))
      thread.setName(s"Worker-$i")
      thread
    }.toList

  /** Start all threads and wait for them to complete */
  def startAndJoinAll(threads: List[Thread]): Unit =
    threads.foreach(_.start())
    threads.foreach(_.join())
