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
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.FiniteDuration

final class AsyncLock private(
  name: String,
  warnTimeouts: IterableOnce[FiniteDuration],
  suppressLog: Boolean)
{
  private val lockM = MVar[Task].empty[Acquirer]().memoize
  private val log = if (suppressLog) js7.base.log.Logger.empty else logger

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
      .flatMap { mvar =>
        val acquirer = new Acquirer(CorrelId.current, acquirerToString)
        mvar.tryPut(acquirer).flatMap(hasAcquired =>
          if (hasAcquired) {
            log.trace(s"↘ $name acquired by $acquirer")
            acquirer.lockedSince = nanoTime()
            Task.unit
          } else
            if (suppressLog)
              mvar.put(acquirer)
                .as(Right(()))
            else {
              val waitingSince = now
              Task.tailRecM(())(_ =>
                mvar.tryRead.flatMap {
                  case Some(lockedBy) =>
                    log.debug(
                      s"⟲ $name enqueues $acquirer (currently locked by $lockedBy) ...")
                    mvar.put(acquirer)
                      .whenItTakesLonger(warnTimeouts)(_ =>
                        for (lockedBy <- mvar.tryRead) yield logger.info(
                          s"$name: ⏳ $acquirer is still waiting" +
                            s" (currently locked by ${lockedBy getOrElse "None"})" +
                            s" for ${waitingSince.elapsed.pretty} ..."))
                      .map { _ =>
                        log.debug(
                          s"↘ $name acquired by $acquirer after ${waitingSince.elapsed.pretty}")
                        acquirer.lockedSince = nanoTime()
                        Right(())
                    }

                  case None =>  // Lock has just become available
                    for (hasAcquired <- mvar.tryPut(acquirer)) yield
                      if (!hasAcquired)
                        Left(())  // Locked again by someone else, so try again
                      else {
                        log.trace(s"↘ $name acquired by $acquirer")
                        acquirer.lockedSince = nanoTime()
                        Right(())  // The lock is ours!
                      }
                })
            })
      }

  private def release(acquirerToString: () => String, exitCase: ExitCase[Throwable]): Task[Unit] =
    Task.defer {
      exitCase match {
        case ExitCase.Completed =>
          log.trace(s"↙ $name released by ${acquirerToString()}")

        case ExitCase.Canceled =>
          log.trace(s"↙❌ $name released by ${acquirerToString()} · Canceled")

        case ExitCase.Error(t) =>
          log.trace(s"↙💥 $name released by ${acquirerToString()} · ${t.toStringWithCauses}")
      }
      lockM.flatMap(_.take).void
    }

  override def toString = s"AsyncLock:$name"
}

object AsyncLock
{
  private val logger = js7.base.log.Logger[this.type]

  def apply()(implicit enclosing: sourcecode.Enclosing): AsyncLock =
    apply(name = enclosing.value)

  def apply(
    name: String,
    logWorryDurations: IterableOnce[FiniteDuration] = DefaultWorryDurations,
    suppressLog: Boolean = false)
  =
    new AsyncLock(name, logWorryDurations, suppressLog)

  private final class Acquirer(correlId: CorrelId, nameToString: () => String) {
    private lazy val name = nameToString()
    var lockedSince: Long = 0

    override def toString =
      if (lockedSince == 0)
        name
      else {
        val c = correlId.fold("", o => s"$o ")
        val duration = (nanoTime() - lockedSince).ns.pretty
        s"$c$name $duration ago"
      }
  }
}
