package js7.launcher.processkiller

import cats.effect.kernel.Resource
import cats.effect.{IO, ResourceIO}
import js7.base.catsutils.FiberVar
import js7.base.io.process.Pid
import js7.base.io.process.ProcessExtensions.isAlive
import js7.base.log.Logger
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.launcher.processkiller.CrashProcessKiller.*
import scala.concurrent.duration.FiniteDuration
import scala.jdk.OptionConverters.*

/** Used by CrashPidFileKiller. */
final class CrashProcessKiller private(
  protected val sigtermDescendantsWatch: FiberVar[Unit],
  override protected val dontExecute: Boolean = false,
  sigkillDelay: FiniteDuration)
extends ProcessKiller[Pid]:

  protected def label = ""

  def killWithDescendants(pids: Seq[Pid]): IO[Unit] =
    IO.defer:
      val alive = pids.filter(_.isAlive)
      IO.whenA(sigkillDelay.isPositive):
        sigtermMainProcessesAndSaveDescendants(alive) *>
          IO.defer:
            logger.info(s"Waiting for sigkillDelay=${sigkillDelay.pretty} ...")
            sigtermDescendantsWatch.joinCurrent.timeoutTo(sigkillDelay, IO.unit)
      .productR:
        sigkillWithDescendants(alive)

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


object CrashProcessKiller:

  private val logger = Logger[this.type]

  def resource(dontExecute: Boolean = false, sigkillDelay: FiniteDuration = 0.s)
  : ResourceIO[CrashProcessKiller] =
    for
      fiberVar <- FiberVar.resource[Unit]
      killer <- Resource.eval(IO:
        CrashProcessKiller(fiberVar, dontExecute, sigkillDelay))
    yield
      killer
