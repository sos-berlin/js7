package js7.executor.forjava.internal

import java.io.{IOException, Writer}
import monix.execution.Ack.{Continue, Stop}
import monix.execution.{Ack, Scheduler}
import monix.reactive.Observer
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Failure

private final class ObserverWriter(observer: Observer[String])(implicit s: Scheduler)
extends Writer {
  self =>

  @volatile var ack: Future[Ack] = Continue

  def write(array: Array[Char], offset: Int, len: Int) =
    write(new String(array, offset, len))

  override def write(string: String) =
    self.synchronized {
      await()
      ack = ack.syncTryFlatten.syncFlatMap {
        case Stop => Stop
        case Continue => observer.onNext(string)
      }
      if (ack == Stop) throw new IOException("Stream closed")
    }

  def flush() =
    self.synchronized {
      await()
    }

  def close() = observer.onComplete()

  private def await(): Unit = {
    Await.ready(ack, Duration.Inf)
    ack.value match {
      case Some(Failure(t: IOException)) => throw t
      case Some(Failure(t)) => throw new IOException(t.toString, t.getCause)
      case _ =>
    }
  }
}
