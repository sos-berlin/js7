package js7.base.stream

import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.monixutils.MonixDeadline
import js7.base.stream.IncreasingNumberSync.*
import js7.base.time.ScalaTime.*
import monix.eval.Task
import org.jetbrains.annotations.TestOnly
import scala.collection.mutable
import scala.concurrent.Promise
import scala.concurrent.duration.Deadline.now
import scala.concurrent.duration.*

/**
  * Synchronizer for increasing numbers.
  */
final class IncreasingNumberSync(initial: Long, valueToString: Long => String)
{
  // TODO Watch size of valueToPromise (for example via an inspection web service)
  private val valueToPromise = mutable.TreeMap[Long, Promise[Unit]]()
  @volatile private var _last = initial

  def onAdded(a: Long): Unit = {
    logger.trace(s"onAdded $a")
    synchronized {
      if (a < _last) throw new IllegalArgumentException(s"Added ${valueToString(a)} < last ${valueToString(_last)}")
      _last = a
      while (valueToPromise.nonEmpty && valueToPromise.firstKey < a) {
        for (promise <- valueToPromise.remove(valueToPromise.firstKey))
          promise.success(())
      }
    }
  }

  // TODO Memory leak when whenAvailable `after` will never be added.
  //  The promise cannot be removed from `valueToPromise` because
  //  it may be in use other calls `whenAvailable` (with different `until`).
  //  No memory leak is expected if used properly.
  /**
    * @param delay When waiting for events, don't succeed after the first event but wait for further events
    */
  def whenAvailable(after: Long, until: Option[MonixDeadline], delay: FiniteDuration = ZeroDuration)
  : Task[Boolean] =
    logger.traceTask(
      Task.defer {
        lazy val logPrefix =
          s"whenAvailable($after delay=${delay.pretty}${until.fold("")(o => " until="+o.timeLeft.pretty)})"
        if (after < _last) {
          logger.trace(s"$logPrefix => fulfilled immediately")
          Task.True  // Event already waiting
        } else if (until.exists(_.hasElapsed)) {
          logger.trace(s"$logPrefix => timed out immediately")
          Task.False  // Timeout
        } else {
          lazy val since = now
          logger.whenTraceEnabled { since; logger.trace(s"$logPrefix ...") }
          val task = whenAvailable2(after)
            .delayResult(delay min until.fold(FiniteDuration.MaxValue)(_.timeLeftOrZero))
          until.fold(task)(u => task.timeoutTo(u.timeLeftOrZero, Task.False))
            .map { isAvailable =>
              logger.trace(
                s"$logPrefix => ${if (isAvailable) "ok" else "timed out"} after ${since.elapsed.pretty}")
              isAvailable
            }
        }
      })

  private def whenAvailable2(after: Long): Task[Boolean] =
    Task.tailRecM(())(_ =>
      if (after < _last)
        RightTrue
      else synchronized {
        if (after < _last)
          RightTrue
        else {
          val promise = valueToPromise.getOrElseUpdate(after, Promise())
          Task.fromFuture(promise.future)
            .as(Left(()))  // Check again
        }
      })

  def last = _last

  @TestOnly
  private[stream] def waitingCount = valueToPromise.size
}

object IncreasingNumberSync {
  private val logger = Logger(getClass)
  private val RightTrue = Task.right(true)
}
