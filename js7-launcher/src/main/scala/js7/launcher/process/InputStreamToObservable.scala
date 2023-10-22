package js7.launcher.process

import cats.effect.{ExitCase, Resource}
import java.io.{InputStream, InputStreamReader, Reader}
import java.nio.charset.Charset
import js7.base.log.Logger
import js7.base.monixutils.UnbufferedReaderStream
import js7.base.thread.IOExecutor
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import cats.effect.IO
import monix.execution.Cancelable
import monix.reactive.{Stream, Observer}
import scala.util.control.NonFatal
import scala.util.{Failure, Success}
import fs2.Stream

object InputStreamToStream:
  private val logger = Logger[this.type]

  def copyInputStreamToStream(
    in: InputStream,
    observer: Observer[String],
    encoding: Charset,
    charBufferSize: Int)
    (implicit iox: IOExecutor)
  : IO[Unit] =
    copyInputStreamToStream(Resource.pure[IO, InputStream](in), observer, encoding,
      charBufferSize)

  def copyInputStreamToStream(
    in: Resource[IO, InputStream],
    observer: Observer[String],
    encoding: Charset,
    charBufferSize: Int)
    (implicit iox: IOExecutor)
  : IO[Unit] =
    copyReaderToStream(
      in.map(new InputStreamReader(_, encoding)),
      observer,
      charBufferSize)

  def copyReaderToStream(reader: Resource[IO, Reader], observer: Observer[String], charBufferSize: Int)
    (implicit iox: IOExecutor)
  : IO[Unit] =
    IO.create[Unit] { (scheduler, callback) =>
      try
        readerToStream(reader, charBufferSize)
          .guaranteeCase:
            case ExitCase.Canceled =>
              logger.debug("Canceled")
              IO(callback(Success(())))
            case _ => IO.unit
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

  def readerToStream(reader: Resource[IO, Reader], charBufferSize: Int)
    (implicit iox: IOExecutor)
  : Stream[IO, String] =
    Stream
      .fromIO(reader.use(reader => IO {
        new UnbufferedReaderStream(reader, charBufferSize)
          .executeOn(iox.scheduler)
      }))
      .flatten
