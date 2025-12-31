package concurrency.ch07

import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}
import scala.concurrent.{Future, ExecutionContext, Await}
import scala.concurrent.duration.Duration

case class Stage(name: String, processor: Any => Any)

class Pipeline[T](stages: List[Stage] = List.empty):

  /** Add a stage to the pipeline */
  def addStage[A, B](name: String, processor: A => B): Pipeline[T] =
    new Pipeline[T](stages :+ Stage(name, x => processor(x.asInstanceOf[A])))

  /** Process data through all stages */
  def process(data: List[T]): List[Any] =
    if stages.isEmpty then return data

    data.map { item =>
      stages.foldLeft[Any](item) { (current, stage) =>
        stage.processor(current)
      }
    }

object Pipeline:
  def apply[T](): Pipeline[T] = new Pipeline[T]()
