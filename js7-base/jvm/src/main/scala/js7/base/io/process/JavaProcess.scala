package js7.base.io.process

import java.io.{InputStream, OutputStream}
import js7.base.utils.ScalaUtils.syntax.*
import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}

final case class JavaProcess(process: Process) extends Js7Process:

  def isAlive: Boolean =
    process.isAlive

  val pid: Pid =
    Pid(process.pid)

  def stdin: OutputStream =
    process.getOutputStream

  def returnCode: Option[ReturnCode] =
    !process.isAlive ? ReturnCode(process.exitValue)

  def destroy(): Unit =
    process.destroy()

  def destroyForcibly(): Unit =
    process.destroyForcibly()

  def waitFor(): ReturnCode =
    ReturnCode(process.waitFor())

  def waitFor(duration: FiniteDuration): Boolean =
    process.waitFor(duration.toMillis, MILLISECONDS)

  def stdout: InputStream =
    process.getInputStream

  def stderr: InputStream =
    process.getErrorStream

  def handle: ProcessHandle =
    process.toHandle

  lazy val maybeHandle: Some[ProcessHandle] =
    Some(process.toHandle)

  override def toString: String =
    pid.toString


object JavaProcess:
  def apply(process: Process) =
    new JavaProcess(process)
