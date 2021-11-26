package js7.executor.process

import cats.effect.{ExitCase, Resource}
import java.io.{InputStream, InputStreamReader, Reader}
import java.nio.charset.StandardCharsets.UTF_8
import js7.base.log.Logger
import js7.base.monixutils.UnbufferedReaderObservable
import js7.base.thread.IOExecutor
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import monix.eval.Task
import monix.execution.Cancelable
import monix.reactive.{Observable, Observer}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

object InputStreamToObservable
{
  private val logger = Logger[this.type]

  def copyInputStreamToObservable(in: InputStream, observer: Observer[String], charBufferSize: Int)
    (implicit iox: IOExecutor)
  : Task[Unit] =
    copyInputStreamToObservable(Resource.pure[Task, InputStream](in), observer, charBufferSize)

  def copyInputStreamToObservable(in: Resource[Task, InputStream], observer: Observer[String], charBufferSize: Int)
    (implicit iox: IOExecutor)
  : Task[Unit] =
    copyReaderToObservable(in.map(new InputStreamReader(_, UTF_8)), observer, charBufferSize)

  def copyReaderToObservable(reader: Resource[Task, Reader], observer: Observer[String], charBufferSize: Int)
    (implicit iox: IOExecutor)
  : Task[Unit] =
    Task.create[Unit] { (scheduler, callback) =>
      try
        readerToObservable(reader, charBufferSize)
          .guaranteeCase {
            case ExitCase.Canceled =>
              logger.debug("Canceled")
              Task(callback(Success(())))
            case _ => Task.unit
          }
          .subscribe(
            new Observer[String] {
              def onNext(o: String) =
                observer.onNext(o)

              def onError(t: Throwable) = {
                logger.debug("⚠️ " + t.toStringWithCauses)
                observer.onError(t)
                callback(Failure(t))
              }

              def onComplete() = {
                observer.onComplete()
                callback(Success(()))
              }
            })(scheduler)
      catch { case NonFatal(t) =>
        callback(Failure(t))
        Cancelable.empty
      }
    }

  def readerToObservable(reader: Resource[Task, Reader], charBufferSize: Int)
    (implicit iox: IOExecutor)
  : Observable[String] =
    Observable
      .fromTask(reader.use(reader => Task {
        new UnbufferedReaderObservable(reader, charBufferSize)
          .executeOn(iox.scheduler)
      }))
      .flatten
}
