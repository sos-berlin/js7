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
  private val lockM = MVar[Task].of(()).memoize

  def lock[A](task: Task[A])(implicit src: sourcecode.Enclosing): Task[A] =
    lock(src.value)(task)

  def lock[A](acquirer: String)(task: Task[A]): Task[A] =
    acquire(acquirer).bracket(_ => task)(_ => release(acquirer))
      // Because cancel() is asynchronous, the use part may continue even though the lock is released.
      // Better we make the whole operation uncancelable.
      // TODO Make cancelable ?
      .uncancelable

  private def acquire(acquirer: String): Task[Unit] =
    lockM.flatMap(mvar =>
      mvar.tryTake
        .flatMap {
          case None =>
            val since = now
            logger.debug(s"$acquirer is waiting for $toString")
            mvar.take
              .whenItTakesLonger(warnTimeouts)(duration => Task {
                logger.info(s"$acquirer is still waiting for $toString for ${duration.pretty} ...")
              })
              .map(_ => logger.debug(s"$acquirer acquired $toString after ${since.elapsed.pretty}"))
          case Some(()) =>
            logger.trace(s"$acquirer acquired $toString")
            Task.unit
        })

  private def release(acquirer: String): Task[Unit] =
    Task.defer {
      logger.trace(s"$acquirer released $toString")
      lockM.flatMap(_.put(()))
    }

  override def toString = s"AsyncLock:$name"
}

object AsyncLock
{
  private val logger = scribe.Logger[this.type]

  def apply(name: String, logWorryDurations: IterableOnce[FiniteDuration] = DefaultWorryDurations) =
    new AsyncLock(name, logWorryDurations)
}
