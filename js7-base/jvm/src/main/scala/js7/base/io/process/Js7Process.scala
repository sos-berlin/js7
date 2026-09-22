package js7.base.io.process

import cats.effect.IO
import java.io.{InputStream, OutputStream}
import js7.base.io.process.Js7Process.*
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.thread.IOExecutor.env.interruptibleVirtualThread
import js7.base.utils.ScalaUtils.syntax.*
import scala.concurrent.duration.FiniteDuration

trait Js7Process:

  def pid: Pid

  def isAlive: Boolean

  def stdin: OutputStream

  def stdout: InputStream

  def stderr: InputStream

  def returnCode: Option[ReturnCode]

  def destroy() : Unit

  def destroyForcibly() : Unit

  /** Await process exit without using [[java.lang.ProcessHandle#onExit]]
    *
    * Because [[java.lang.ProcessHandle#onExit]] may block while reading stdout or stderr,
    * ourOnExit is implemented with [[waitFor]] in a blocking thread.
    */
  def ourOnExit: IO[ReturnCode] =
    interruptibleVirtualThread:
      logger.debugCallWithResult(s"waitFor $this"):
        waitFor()

  def waitFor(): ReturnCode

  def waitFor(duration: FiniteDuration): Boolean

  def maybeHandle: Option[ProcessHandle]

  def release: IO[Unit]

  override def toString: String =
    s"$pid${!isAlive ?? "†"}"


object Js7Process:
  val logger = Logger[this.type]
