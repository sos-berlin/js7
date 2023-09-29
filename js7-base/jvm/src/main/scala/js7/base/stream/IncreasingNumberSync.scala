package js7.base.stream

import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax.*
import js7.base.monixutils.MonixDeadline
import js7.base.stream.IncreasingNumberSync.*
import js7.base.time.ScalaTime.*
import monix.eval.Task
import org.jetbrains.annotations.TestOnly
import scala.collection.mutable
import scala.concurrent.Promise
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
    //logger.trace(s"onAdded $a")
    synchronized {
      if a < _last then throw new IllegalArgumentException(s"Added ${valueToString(a)} < last ${valueToString(_last)}")
      _last = a
      while valueToPromise.nonEmpty && valueToPromise.firstKey < a do {
        for promise <- valueToPromise.remove(valueToPromise.firstKey) do
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
  : Task[Boolean] = {
    //def argsString =
    //  s"$after delay=${delay.pretty}${until.fold("")(o => " until=" + o.timeLeft.pretty)}"
    //logger.traceTaskWithResult("whenAvailable", argsString, task =
      Task.defer {
        if after < _last then
          Task.True  // Event already waiting
        else if until.exists(_.hasElapsed) then
          Task.False  // Timeout
        else {
          val task = whenAvailable2(after)
            .delayResult(delay min until.fold(FiniteDuration.MaxValue)(_.timeLeftOrZero))
          until.fold(task)(u => task.timeoutTo(u.timeLeftOrZero, Task.False))
        }
      }
  }

  private def whenAvailable2(after: Long): Task[Boolean] =
    Task.tailRecM(())(_ =>
      if after < _last then
        RightTrue
      else synchronized {
        if after < _last then
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
  private val logger = Logger[this.type]
  private val RightTrue = Task.right(true)
}
