package js7.base.utils

import cats.effect.ExitCase
import java.lang.System.nanoTime
import js7.base.log.CorrelId
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
  noMinorLog: Boolean = false)
{
  private val lockM = MVar[Task].empty[Acquirer]().memoize
  private val log = if (noLog) js7.base.log.Logger.empty else logger

  def lock[A](task: Task[A])(implicit src: sourcecode.Enclosing): Task[A] =
    lock(src.value)(task)

  def lock[A](acquirer: => String)(task: Task[A]): Task[A] = {
    lazy val acq = acquirer
    lock2(() => acq)(task)
  }

  private def lock2[A](acquirer: () => String)(task: Task[A]): Task[A] =
    acquire(acquirer)
      .bracketCase(_ => task)((_, exitCase) => release(acquirer, exitCase))

  private def acquire(acquirerToString: () => String): Task[Unit] =
    lockM
      .flatMap(mvar => Task.defer {
        val acquirer = new Acquirer(CorrelId.current, acquirerToString)
        mvar.tryPut(acquirer).flatMap(hasAcquired =>
          if (hasAcquired) {
            if (!noMinorLog) log.trace(s"↘ $name acquired by $acquirer ↘")
            acquirer.startMetering()
            Task.unit
          } else
            if (noLog)
              mvar.put(acquirer)
                .as(Right(()))
            else {
              val waitingSince = now
              Task.tailRecM(())(_ =>
                mvar.tryRead.flatMap {
                  case Some(lockedBy) =>
                    val nr = waitCounter.incrementAndGet()
                    var infoLogged = false
                    log.debug(/*spaces are for column alignment*/
                      s"⟲ 🟡$nr $name enqueues    $acquirer (currently locked by ${lockedBy.withCorrelId}) ...")
                    mvar.put(acquirer)
                      .whenItTakesLonger(warnTimeouts)(_ =>
                        for (lockedBy <- mvar.tryRead) yield {
                          val m = if (!infoLogged) "🟠" else "🔴"
                          infoLogged = true
                          logger.info(
                            s"⟲ $m$nr $name: $acquirer is still waiting" +
                              s" for ${waitingSince.elapsed.pretty}," +
                              s" currently locked by ${lockedBy getOrElse "None"} ...")
                        })
                      .map { _ =>
                        lazy val msg =
                          s"↘ 🟢$nr $name acquired by $acquirer after ${waitingSince.elapsed.pretty} ↘"
                        if (infoLogged) log.info(msg) else log.debug(msg)
                        acquirer.startMetering()
                        Right(())
                    }

                  case None =>  // Lock has just become available
                    for (hasAcquired <- mvar.tryPut(acquirer)) yield
                      if (!hasAcquired)
                        Left(())  // Locked again by someone else, so try again
                      else {
                        if (!noMinorLog) log.trace(s"↘ $name acquired by $acquirer ↘")
                        acquirer.startMetering()
                        Right(())  // The lock is ours!
                      }
                })
            })
      })

  private def release(acquirerToString: () => String, exitCase: ExitCase[Throwable]): Task[Unit] =
    Task.defer {
      exitCase match {
        case ExitCase.Completed =>
          if (!noMinorLog) log.trace(s"↙ $name released by ${acquirerToString()} ↙")

        case ExitCase.Canceled =>
          if (!noMinorLog) log.trace(s"↙❌ $name released by ${acquirerToString()} · Canceled ↙")

        case ExitCase.Error(t) =>
          if (!noMinorLog) log.trace(s"↙💥 $name released by ${acquirerToString()} · ${t.toStringWithCauses} ↙")
      }
      lockM.flatMap(_.take).void
    }

  override def toString = s"AsyncLock:$name"
}

object AsyncLock
{
  private val logger = js7.base.log.Logger[this.type]
  private val waitCounter = Atomic(0)

  def apply()(implicit enclosing: sourcecode.Enclosing): AsyncLock =
    apply(noMinorLog = false)

  def apply(noMinorLog: Boolean)(implicit enclosing: sourcecode.Enclosing): AsyncLock =
    apply(name = enclosing.value, noMinorLog = noMinorLog)

  def apply(
    name: String,
    logWorryDurations: IterableOnce[FiniteDuration] = DefaultWorryDurations,
    suppressLog: Boolean = false,
    noMinorLog: Boolean = false)
  : AsyncLock =
    new AsyncLock(name, logWorryDurations, suppressLog, noMinorLog = noMinorLog)

  private final class Acquirer(correlId: CorrelId, nameToString: () => String) {
    private lazy val name = nameToString()
    private var lockedSince: Long = 0

    def withCorrelId: String =
      if (lockedSince == 0)
        name
      else
        correlId.fold("", o => s"$o ") + toString

    def startMetering(): Unit =
      lockedSince = nanoTime()

    override def toString =
      if (lockedSince == 0)
        name
      else {
        val duration = (nanoTime() - lockedSince).ns.pretty
        s"$name $duration ago"
      }
  }
}
