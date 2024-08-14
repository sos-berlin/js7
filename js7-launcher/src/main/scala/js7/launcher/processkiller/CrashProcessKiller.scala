package js7.launcher.processkiller

import cats.effect.IO
import js7.base.io.process.Pid
import js7.base.utils.ScalaUtils.*
import js7.base.utils.ScalaUtils.syntax.*
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

  protected def killMainProcessOnly(pid: Pid, force: Boolean): IO[Unit] =
    pid.maybeProcessHandle.fold(IO.unit): h =>
      killProcessHandle(h, force = force)

  protected def killingLogLine(
    processHandle: ProcessHandle,
    force: Boolean,
    isDescendant: Boolean,
    isLast: Boolean)
  : String =
    "❌ " +
      (if force then """destroyForcibly (SIGKILL) """ else """destroy (SIGTERM) """) +
      (isDescendant ?? (if isLast then "└ " else "├ ")) +
      Pid(processHandle.pid) +
      ' ' +
      processHandle.info.commandLine.toScala.getOrElse("")
