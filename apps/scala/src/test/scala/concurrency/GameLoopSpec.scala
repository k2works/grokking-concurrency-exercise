package concurrency

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import concurrency.ch06.{GameLoop, GameTask, ProcessorFreeEvent}
import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.*

class GameLoopSpec extends AnyFunSuite with Matchers:

  test("GameTask should execute its action"):
    var executed = false
    val task = GameTask("TestTask", () => executed = true)
    task.run()
    executed shouldBe true

  test("ProcessorFreeEvent should block until signaled"):
    val event = new ProcessorFreeEvent
    val results = new ConcurrentLinkedQueue[String]()

    val waiter = new Thread(new Runnable:
      def run(): Unit =
        event.waitForSignal()
        results.add("waited")
    )

    waiter.start()
    Thread.sleep(50)
    results.size shouldBe 0 // Still waiting

    event.signal()
    waiter.join(1000)
    results.asScala.toList shouldBe List("waited")

  test("GameLoop should run multiple tasks"):
    val results = new ConcurrentLinkedQueue[String]()
    val tasks = List(
      GameTask("Task1", () => results.add("task1")),
      GameTask("Task2", () => results.add("task2")),
      GameTask("Task3", () => results.add("task3"))
    )

    GameLoop.runOneFrame(tasks)
    results.size shouldBe 3

  test("GameLoop should run tasks in order"):
    val results = new ConcurrentLinkedQueue[String]()
    val tasks = List(
      GameTask("First", () => results.add("1")),
      GameTask("Second", () => results.add("2")),
      GameTask("Third", () => results.add("3"))
    )

    GameLoop.runOneFrame(tasks)
    results.asScala.toList shouldBe List("1", "2", "3")
