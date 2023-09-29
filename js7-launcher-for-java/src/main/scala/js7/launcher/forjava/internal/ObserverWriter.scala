package js7.launcher.forjava.internal

import java.io.{IOException, Writer}
import monix.execution.Ack.{Continue, Stop}
import monix.execution.{Ack, UncaughtExceptionReporter}
import monix.reactive.Observer
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}

private final class ObserverWriter(observer: Observer[String])
  (implicit u: UncaughtExceptionReporter)
extends Writer:
  self =>

  private var ack: Future[Ack] = Continue

  def write(array: Array[Char], offset: Int, len: Int) =
    write(new String(array, offset, len))

  override def write(string: String) =
    self.synchronized:
      await()
      ack = ack.syncOnContinue(observer.onNext(string))

  def flush() =
    self.synchronized:
      await()

  def close() = observer.onComplete()

  private def await(): Unit =
    Await.ready(ack, Duration.Inf)
    ack.value match
      case Some(Failure(t: IOException)) => throw t
      case Some(Failure(t)) => throw new IOException(t.toString, t.getCause)
      case Some(Success(Stop)) => throw new IOException("Stream closed")
      case _ =>
