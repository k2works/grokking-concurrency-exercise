package concurrency

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import concurrency.ch04.ThreadBasics
import java.util.concurrent.ConcurrentLinkedQueue

class ThreadBasicsSpec extends AnyFunSuite with Matchers:

  test("createWorker should create a thread that runs the given task"):
    var executed = false
    val thread = ThreadBasics.createWorker("TestWorker", () => executed = true)
    thread.start()
    thread.join()
    executed shouldBe true

  test("createWorker should set thread name"):
    val thread = ThreadBasics.createWorker("MyWorker", () => ())
    thread.getName shouldBe "MyWorker"

  test("createMultipleWorkers should create specified number of threads"):
    val threads = ThreadBasics.createMultipleWorkers(5, _ => ())
    threads.length shouldBe 5

  test("createMultipleWorkers threads should execute with correct indices"):
    val results = new ConcurrentLinkedQueue[Int]()
    val threads = ThreadBasics.createMultipleWorkers(3, i => results.add(i))
    threads.foreach(_.start())
    threads.foreach(_.join())
    import scala.jdk.CollectionConverters.*
    results.asScala.toSet shouldBe Set(0, 1, 2)

  test("startAndJoinAll should run all threads"):
    val results = new ConcurrentLinkedQueue[String]()
    val threads = ThreadBasics.createMultipleWorkers(3, i => results.add(s"Worker-$i"))
    ThreadBasics.startAndJoinAll(threads)
    results.size shouldBe 3
