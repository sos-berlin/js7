package js7.launcher.processkiller

import cats.effect.{IO, Resource, ResourceIO}
import js7.base.catsutils.FiberVar
import js7.base.io.process.{Js7Process, Pid}
import js7.base.utils.ScalaUtils.*
import js7.base.utils.ScalaUtils.syntax.*
import scala.jdk.OptionConverters.*

private[launcher] final class SubagentProcessKiller private(
  protected val sigtermDescendantsWatch: FiberVar[Unit],
  protected val label: String)
extends ProcessKiller[Js7Process]:

  def sigtermMainProcessAndSaveDescendant(process: Js7Process): IO[Unit] =
    IO.whenA(process.isAlive):
      sigtermMainProcessesAndSaveDescendants(Seq(process))

  def sigkillWithDescendants(process: Js7Process): IO[Unit] =
    sigkillWithDescendants(Seq(process))

  protected def killingLogLine(
    processHandle: ProcessHandle,
    force: Boolean,
    isDescendant: Boolean,
    isLast: Boolean)
  : String =
    (if isDescendant then "❌ " else "◼️  ") +
      (if force then """destroyForcibly (SIGKILL) """ else """destroy (SIGTERM) """) +
      (isDescendant ?? (
        "child process " + Pid(processHandle.pid) +
          ' ' + processHandle.info.commandLine.toScala.getOrElse("")))


object SubagentProcessKiller:

  def resource(label: String): ResourceIO[SubagentProcessKiller] =
    for
      fiberVar <- FiberVar.resource[Unit]
      killer <- Resource.eval(IO:
        SubagentProcessKiller(fiberVar, label))
    yield
      killer
