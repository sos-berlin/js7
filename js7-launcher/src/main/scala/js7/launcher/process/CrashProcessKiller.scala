package js7.launcher.process

import cats.effect.IO
import js7.base.io.process.Pid
import js7.base.utils.ScalaUtils.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.launcher.process.ProcessKiller.{descendants, maybeProcessHandle}
import scala.jdk.OptionConverters.*

final class CrashProcessKiller(
  override protected val dontExecute: Boolean = false)
extends ProcessKiller[Pid]:

  protected def label = ""

  def sigkillWithDescendants(pids: Seq[Pid]): IO[Unit] =
    genericSigkillWithDescendants:
      pids
        .filter(_.isAlive)
        .map: pid =>
          pid -> pid.descendants

  def killMainProcessOnly(pid: Pid, force: Boolean): IO[Unit] =
    pid.maybeProcessHandle.fold(IO.unit): h =>
      killViaProcessHandle(h, force = force)

  protected def killingLogLine(
    processHandle: ProcessHandle,
    force: Boolean,
    isDescendant: Boolean,
    isLastDescendant: Boolean)
  : String =
    "❌ " +
      (if force then """destroyForcibly (SIGKILL) """ else """destroy (SIGTERM) """) +
      (isDescendant ?? (if isLastDescendant then "└ " else "├ ")) +
      Pid(processHandle.pid) +
      ' ' +
      processHandle.info.commandLine.toScala.getOrElse("")
