package js7.launcher

import js7.base.io.process.{Stderr, Stdout, StdoutOrStderr}
import js7.base.monixutils.TaskObserver
import js7.base.utils.ScalaUtils.syntax.*
import js7.launcher.utils.KeepLastLineObserver
import cats.effect.IO
import monix.reactive.Observer

final class StdObservers(
  val out: Observer[String],
  err_ : Observer[String],
  val charBufferSize: Int,
  keepLastErrLine: Boolean):

  private val lastLineErr = keepLastErrLine ? new KeepLastLineObserver(err_)
  val err: Observer[String] = lastLineErr.getOrElse(err_)

  def errorLine: Option[String] =
    lastLineErr.flatMap(_.lastLine)

  val outIOObserver = TaskObserver(out)
  val errIOObserver = TaskObserver(err)

  def ioObserver(outerr: StdoutOrStderr): TaskObserver[String] =
    outerr match
      case Stdout => outIOObserver
      case Stderr => errIOObserver

  val stop: IO[Unit] =
    IO.parZip2(outIOObserver.complete, errIOObserver.complete)
      .void
      .memoize
