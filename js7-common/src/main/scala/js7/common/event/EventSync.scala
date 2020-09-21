package js7.common.event

import js7.base.monixutils.MonixBase.syntax._
import js7.base.monixutils.MonixDeadline
import js7.base.time.ScalaTime._
import monix.eval.Task
import scala.collection.mutable
import scala.concurrent.Promise
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class EventSync(initial: Long, valueToString: Long => String)
{
  // TODO Watch size of valueToPromise (for example via an inspection web service)
  private val valueToPromise = mutable.TreeMap[Long, Promise[Boolean]]()
  @volatile private var _last = initial

  def onAdded(a: Long): Unit =
    synchronized {
      if (a < _last) throw new IllegalArgumentException(s"Added ${valueToString(a)} < last ${valueToString(_last)}")
      _last = a
      while (valueToPromise.nonEmpty && valueToPromise.firstKey < a) {
        for (promise <- valueToPromise.remove(valueToPromise.firstKey))
          promise.success(true)
      }
    }

  // TODO Memory leak when whenAvailable `after` will never be added.
  //  The promise cannot be removed from `valueToPromise` because
  //  it may be in use other calls `whenAvailable` (with different `until`).
  //  No memory leak is expected if used properly.
  /**
    * @param delay When waiting for events, don't succeed after the first event but wait for further events
    */
  def whenAvailable(after: Long, until: Option[MonixDeadline], delay: FiniteDuration = Duration.Zero): Task[Boolean] =
    if (after < _last)
      Task.True  // Event already waiting
    else if (until.exists(_.hasElapsed))
      Task.False  // Timeout
    else {
      val task = whenAvailable2(after)
        .delayResult(delay min until.fold(FiniteDuration.MaxValue)(_.timeLeftOrZero))
      until.fold(task)(u => task.timeoutTo(u.timeLeftOrZero, Task.False))
    }

  private def whenAvailable2(after: Long): Task[Boolean] =
    Task.tailRecM(()) { _ =>
      if (after < _last)
        Task.pure(Right(true))
      else synchronized {
        if (after < _last)
          Task.pure(Right(true))
        else {
          val promise = valueToPromise.get(after) match {
            case Some(o) => o
            case None =>
              val p = Promise[Boolean]()
              valueToPromise.put(after, p)
              p
          }
          Task.fromFuture(promise.future) >>
            Task.pure(Left(()))  // Check again
        }
      }
    }

  def last = _last

  private[event] def waitingCount = valueToPromise.size
}
