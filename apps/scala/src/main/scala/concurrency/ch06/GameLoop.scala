package concurrency.ch06

case class GameTask(name: String, action: () => Unit):
  def run(): Unit = action()

class ProcessorFreeEvent:
  private val lock = new Object
  @volatile private var signaled = false

  def waitForSignal(): Unit = lock.synchronized {
    while !signaled do lock.wait()
  }

  def signal(): Unit = lock.synchronized {
    signaled = true
    lock.notifyAll()
  }

  def reset(): Unit = lock.synchronized {
    signaled = false
  }

object GameLoop:
  /** Run all tasks in a single frame */
  def runOneFrame(tasks: List[GameTask]): Unit =
    tasks.foreach(_.run())

  /** Run game loop for specified number of frames */
  def run(tasks: List[GameTask], frames: Int): Unit =
    (1 to frames).foreach { frame =>
      println(s"Frame $frame")
      runOneFrame(tasks)
    }
