package js7.launcher.process

import cats.effect.IO
import js7.base.io.process.{JavaProcess, Js7Process, Pid}
import js7.base.time.ScalaTime.ZeroDuration
import js7.base.utils.ScalaUtils.*
import js7.base.utils.ScalaUtils.syntax.*
import scala.collection.mutable
import scala.collection.immutable.VectorBuilder
import scala.jdk.OptionConverters.*
import scala.jdk.CollectionConverters.*
import js7.launcher.process.ProcessKiller.descendants
import scala.concurrent.duration.Deadline

final class SubagentProcessKiller(
  protected val label: String)
extends ProcessKiller[Js7Process]:

  private var sigtermDescendants: Seq[ProcessHandle] = Nil
  private var sigtermDescendantsSince = Deadline(ZeroDuration)

  def sigkillWithDescendants(process: Js7Process): IO[Unit] =
    genericSigkillWithDescendants:
      Seq:
        process ->
          // sigtermDescendants may be old. The OS must not reuse PIDs too early.
          (descendantsOf(sigtermDescendants) ++: process.descendants).distinct

  def killMainProcessOnly(process: Js7Process, force: Boolean): IO[Unit] =
    IO.defer:
      IO.whenA(process.isAlive):
        if !force then
          sigtermDescendants = process.descendants // Maybe we must sigkill them later
          sigtermDescendantsSince = Deadline.now

        process.match
          case process: JavaProcess =>
            // Kill via ProcessHandle because this doesn't close stdout and stderr
            killViaProcessHandle(process.handle, force = force)
          case _ =>
            killWithJava(process, force)

  protected def killingLogLine(
    processHandle: ProcessHandle,
    force: Boolean,
    isDescendant: Boolean,
    isLastDescendant: Boolean)
  : String =
    (if isDescendant then "❌ " else "⚫️ ") +
      (if force then """destroyForcibly (SIGKILL) """ else """destroy (SIGTERM) """) +
      (isDescendant ?? (
        "child process " +
          Pid(processHandle.pid) +
          ' ' +
          processHandle.info.commandLine.toScala.getOrElse("")))

  /** Return a Seq of distinct ProcessHandles */
  private def descendantsOf(processHandles: Seq[ProcessHandle]): Seq[ProcessHandle] =
    val known = mutable.Set[Long]()
    val result = VectorBuilder[ProcessHandle]()
    for p <- processHandles if p.isAlive do
      if !known(p.pid) then
        known += p.pid
        result += p
        for d <- p.descendants().iterator().asScala do
          if !known(d.pid) then
            known += d.pid
            result += d
    result.result()
