package js7.base.io.process

import js7.base.io.process.Processes.{processToPidOption, processToString}
import js7.base.utils.ScalaUtils.syntax.*
import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}

final class JavaProcess(process: Process) extends Js7Process:

  def isAlive =
    process.isAlive

  def pid =
    processToPidOption(process)

  def stdin =
    process.getOutputStream

  def returnCode =
    !process.isAlive ? ReturnCode(process.exitValue)

  def destroy() =
    process.destroy()

  def destroyForcibly() =
    process.destroyForcibly()

  def waitFor() =
    ReturnCode(process.waitFor())

  def waitFor(duration: FiniteDuration) =
    process.waitFor(duration.toMillis, MILLISECONDS)

  def stdout =
    process.getInputStream

  def stderr =
    process.getErrorStream

  override def toString = processToString(process, pid)

object JavaProcess:
  def apply(process: Process) =
    new JavaProcess(process)
