package js7.base.utils

import cats.effect.{ExitCase, Resource}
import java.lang.System.nanoTime
import js7.base.log.Logger.syntax.*
import js7.base.log.{BlockingSymbol, CorrelId, Logger}
import js7.base.monixutils.MonixBase.DefaultWorryDurations
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.AsyncLock.*
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import monix.catnap.MVar
import monix.eval.Task
import monix.execution.atomic.Atomic
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.FiniteDuration

final class AsyncLock private(
  name: String,
  warnTimeouts: IterableOnce[FiniteDuration],
  noLog: Boolean,
  logMinor: Boolean):
  asyncLock =>

  private val lockM = MVar[Task].empty[Locked]().memoize
  private val log = if noLog then Logger.empty else logger

  def lock[A](task: Task[A])(implicit src: sourcecode.Enclosing): Task[A] =
    lock(src.value)(task)

  def lock[A](acquirer: => String)(task: Task[A]): Task[A] =
    resource(acquirer).use(_ => task)

  def resource(implicit src: sourcecode.Enclosing): Resource[Task, Locked] =
    resource(src.value)

  def resource(acquirer: => String): Resource[Task, Locked] =
    Resource.makeCase(
      acquire = Task.defer {
        val locked = new Locked(CorrelId.current, waitCounter.incrementAndGet(), acquirer)
        acquire(locked).as(locked)
      })(
      release = (locked, exitCase) =>
        release(locked, exitCase))

  private def acquire(locked: Locked): Task[Unit] =
    lockM.flatMap(mvar => Task.defer {
      mvar.tryPut(locked).flatMap(hasAcquired =>
        if hasAcquired then {
          if logMinor then log.trace(s"↘ ⚪️${locked.nr} $name acquired by ${locked.who} ↘")
          locked.startMetering()
          Task.unit
        } else
          if noLog then
            mvar.put(locked)
              .as(Right(()))
          else {
            val waitingSince = now
            Task.tailRecM(())(_ =>
              mvar.tryRead.flatMap {
                case Some(lockedBy) =>
                  val sym = new BlockingSymbol
                  sym.onDebug()
                  log.debug(/*spaces are for column alignment*/
                    s"⟲ $sym${locked.nr} $name enqueues    ${locked.who} (currently acquired by ${lockedBy.withCorrelId}) ...")
                  mvar.put(locked)
                    .whenItTakesLonger(warnTimeouts) { _ =>
                      for lockedBy <- mvar.tryRead yield 
                        sym.onInfo()
                        logger.info(
                          s"⟲ $sym${locked.nr} $name: ${locked.who} is still waiting" +
                            s" for ${waitingSince.elapsed.pretty}," +
                            s" currently acquired by ${lockedBy getOrElse "None"} ...")
                    }
                    .map { _ =>
                      log.log(sym.releasedLogLevel,
                        s"↘ 🟢${locked.nr} $name acquired by ${locked.who} after ${waitingSince.elapsed.pretty} ↘")
                      locked.startMetering()
                      Right(())
                  }

                case None =>  // Lock has just become available
                  for hasAcquired <- mvar.tryPut(locked) yield
                    if !hasAcquired then
                      Left(())  // Locked again by someone else, so try again
                    else {
                      // "…" denotes just-in-time availability
                      if logMinor then log.trace(s"↘ ⚪️${locked.nr} $name acquired by…${locked.who} ↘")
                      locked.startMetering()
                      Right(())  // The lock is ours!
                    }
              })
          })
    })

  private def release(locked: Locked, exitCase: ExitCase[Throwable]): Task[Unit] =
    Task.defer:
      logRelease(locked, exitCase)
      lockM.flatMap(_.take).void

  private def logRelease(locked: Locked, exitCase: ExitCase[Throwable]): Unit =
    if logMinor then exitCase match
      case ExitCase.Completed =>
        log.trace(s"↙ ⚪️${locked.nr} $name released by ${locked.acquirer} ↙")

      case ExitCase.Canceled =>
        log.trace(s"↙ ⚫${locked.nr} $name released by ${locked.acquirer} · Canceled ↙")

      case ExitCase.Error(t) =>
        log.trace(s"↙ 💥${locked.nr} $name released by ${locked.acquirer} · ${t.toStringWithCauses} ↙")

  override def toString = s"AsyncLock:$name"

  final class Locked private[AsyncLock](
    correlId: CorrelId,
    private[AsyncLock] val nr: Int,
    acquirerToString: => String):
    private[AsyncLock] lazy val acquirer = acquirerToString
    private var lockedSince: Long = 0

    private[AsyncLock] def withCorrelId: String =
      if lockedSince == 0 then
        acquirer
      else
        correlId.fold("", o => s"$o ") + who

    private[AsyncLock] def startMetering(): Unit =
      lockedSince = nanoTime()

    private[AsyncLock] def who: String =
      if lockedSince == 0 then
        acquirer
      else
        val duration = (nanoTime() - lockedSince).ns.pretty
        s"$acquirer $duration ago"

    override def toString =
      s"$asyncLock acquired by $who"

object AsyncLock:
  private val logger = Logger[this.type]
  private val waitCounter = Atomic(0)

  def apply()(implicit enclosing: sourcecode.Enclosing): AsyncLock =
    apply(logMinor = false)

  def apply(logMinor: Boolean)(implicit enclosing: sourcecode.Enclosing): AsyncLock =
    apply(name = enclosing.value, logMinor = logMinor)

  def apply(
    name: String,
    logWorryDurations: IterableOnce[FiniteDuration] = DefaultWorryDurations,
    suppressLog: Boolean = false,
    logMinor: Boolean = false)
  : AsyncLock =
    new AsyncLock(name, logWorryDurations, suppressLog, logMinor = logMinor)
