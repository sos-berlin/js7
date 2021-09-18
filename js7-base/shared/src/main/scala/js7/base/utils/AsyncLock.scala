package js7.base.utils

import js7.base.monixutils.MonixBase.DefaultWorryDurations
import js7.base.monixutils.MonixBase.syntax._
import js7.base.time.ScalaTime._
import js7.base.utils.AsyncLock._
import monix.catnap.MVar
import monix.eval.Task
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.FiniteDuration

final class AsyncLock private(name: String, warnTimeouts: IterableOnce[FiniteDuration])
{
  private val lockM = MVar[Task].empty[String]().memoize

  def lock[A](task: Task[A])(implicit src: sourcecode.Enclosing): Task[A] =
    lock(src.value)(task)

  def lock[A](acquirer: String)(task: Task[A]): Task[A] =
    acquire(acquirer).bracket(_ => task)(_ => release(acquirer))
      // Because cancel() is asynchronous, the use part may continue even though
      // the lock is released (?). Better we make the whole operation uncancelable.
      // TODO Make cancelable ?
      .uncancelable

  private def acquire(acquirer: String): Task[Unit] =
    lockM
      .flatMap(mvar =>
        mvar.tryPut(acquirer).flatMap(hasAcquired =>
          if (hasAcquired)
            Task.unit
          else {
            val since = now
            Task.tailRecM(())(_ =>
              mvar.tryRead.flatMap {
                case Some(lockedBy) =>
                  logger.debug(s"$acquirer is waiting for $toString (currently locked by $lockedBy)")
                  mvar.put(acquirer)
                    .whenItTakesLonger(warnTimeouts)(duration =>
                      for (lockedBy <- mvar.tryRead) yield logger.info(
                        s"$acquirer is still waiting for $toString" +
                          s" (currently locked by ${lockedBy.getOrElse("None")})" +
                          s" for ${duration.pretty} ..."))
                    .map { _ =>
                      logger.debug(s"$acquirer acquired $toString after ${since.elapsed.pretty}")
                      Right(())
                    }
                case None =>  // Lock has just become available
                  for (hasAcquired <- mvar.tryPut(acquirer)) yield
                    if (!hasAcquired)
                      Left(())  // Locked again by someone else, so try again
                    else
                      Right(())  // The lock is ours!
              })
          }))
      .map(_ => logger.trace(s"$acquirer acquired $toString"))

  private def release(acquirer: String): Task[Unit] =
    Task.defer {
      logger.trace(s"$acquirer releases $toString")
      lockM.flatMap(_.take).void
    }

  override def toString = s"AsyncLock:$name"
}

object AsyncLock
{
  private val logger = scribe.Logger[this.type]

  def apply(name: String, logWorryDurations: IterableOnce[FiniteDuration] = DefaultWorryDurations) =
    new AsyncLock(name, logWorryDurations)
}
