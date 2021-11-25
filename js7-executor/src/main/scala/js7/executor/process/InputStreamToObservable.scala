package js7.executor.process

import cats.effect.ExitCase
import java.io.{InputStream, InputStreamReader, Reader}
import java.nio.charset.StandardCharsets.UTF_8
import js7.base.monixutils.UnbufferedReaderObservable
import js7.base.thread.IOExecutor
import monix.eval.{Fiber, Task}
import monix.reactive.Observer
import scala.concurrent.Promise

object InputStreamToObservable
{
  def copyInputStreamToObservable(in: InputStream, observer: Observer[String], charBufferSize: Int)
    (implicit iox: IOExecutor)
  : Task[Fiber[Unit]] =
    copyReaderToObservable(new InputStreamReader(in, UTF_8), observer, charBufferSize)

  def copyReaderToObservable(reader: Reader, observer: Observer[String], charBufferSize: Int)
    (implicit iox: IOExecutor)
  : Task[Fiber[Unit]] =
    Task.deferAction { implicit s =>
      Task {
        val onTerminated = Promise[Unit]()
        val cancelable = new UnbufferedReaderObservable(reader, charBufferSize)
          .executeOn(iox.scheduler)
          .guaranteeCase {
            case ExitCase.Error(t) => Task(onTerminated.failure(t))
            case ExitCase.Completed | ExitCase.Canceled => Task(onTerminated.success(()))
          }
          .subscribe(observer)

        Fiber(
          Task.fromFuture(onTerminated.future),
          Task(cancelable.cancel()))
      }.onErrorHandle(t =>
        Fiber(Task.raiseError(t), Task.unit))
    }
}
