package js7.base.utils

import js7.base.monixutils.MonixBase.DefaultWorryDurations
import js7.base.monixutils.MonixBase.syntax._
import js7.base.time.ScalaTime._
import monix.catnap.MVar
import monix.eval.Task
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.FiniteDuration

final class TaskLock private(name: String, warnTimeouts: IterableOnce[FiniteDuration])
{
  private val lockM = MVar[Task].of(()).memoize

  def lock[A](task: Task[A])(implicit src: sourcecode.Enclosing): Task[A] =
    acquire.bracket(_ => task)(_ => release)
      // Because cancel() is asynchronous the use part may continue even thought the lock is released.
      // Better we make the whole operation uncancelable.
      .uncancelable

  private def acquire(implicit src: sourcecode.Enclosing) =
    lockM.flatMap(mvar =>
      mvar.tryTake
        .flatMap {
          case None =>
            scribe.debug(s"${src.value} is waiting for $toString")
            val since = now
            mvar.take
              .whenItTakesLonger(warnTimeouts)(duration => Task {
                scribe.info(s"${src.value} is still waiting for $toString for ${duration.pretty} ...")
              })
              .tapEval(_ => Task(
                scribe.debug(s"${src.value} acquired $toString after ${since.elapsed.pretty}")))
          case Some(()) =>
            scribe.trace(s"${src.value} acquired $toString")
            Task.unit
        })

  private def release(implicit src: sourcecode.Enclosing) =
    Task.defer {
      scribe.trace(s"${src.value} released $toString")
      lockM.flatMap(_.put(()))
    }

  override def toString = s"TaskLock:$name"
}

object TaskLock
{
  def apply(name: String, logWorryDurations: IterableOnce[FiniteDuration] = DefaultWorryDurations) =
    new TaskLock(name, logWorryDurations)
}
