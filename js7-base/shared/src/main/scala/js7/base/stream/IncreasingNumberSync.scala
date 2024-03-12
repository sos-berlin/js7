package js7.base.stream

import cats.effect.IO
import cats.syntax.traverse.*
import js7.base.catsutils.CatsDeadline
import js7.base.catsutils.CatsEffectExtensions.*
import js7.base.stream.IncreasingNumberSync.*
import js7.base.time.ScalaTime.*
import org.jetbrains.annotations.TestOnly
import scala.collection.mutable
import scala.concurrent.Promise
import scala.concurrent.duration.*

/**
  * Synchronizer for increasing numbers.
  */
final class IncreasingNumberSync(initial: Long, valueToString: Long => String):

  // TODO Watch size of valueToPromise (for example via an inspection web service)
  private val valueToPromise = mutable.TreeMap[Long, Promise[Unit]]()
  @volatile private var _last = initial

  def onAdded(a: Long): Unit =
    //logger.trace(s"onAdded $a")
    synchronized:
      if a < _last then throw new IllegalArgumentException(s"Added ${valueToString(a)} < last ${valueToString(_last)}")
      _last = a
      while valueToPromise.nonEmpty && valueToPromise.firstKey < a do
        for promise <- valueToPromise.remove(valueToPromise.firstKey) do
          promise.success(())

  /** Wait until the expected EventId has occurred or overpaced.
   *
   * Call whenAvailable only with an `after` value which will be arrived,
   * for only then the registered promise will be removed again.
   * Do not call with `after` values in far future, which will never be arrived./**
    * @param delay When waiting for events, don't succeed after the first event but wait for further events
    */
   */
  def whenAvailable(after: Long, until: Option[CatsDeadline], delay: FiniteDuration = ZeroDuration)
  : IO[Boolean] =
    //def argsString =
    //  s"$after delay=${delay.pretty}${until.fold("")(o => " until=" + o.timeLeft.pretty)}"
    //logger.traceIOWithResult("whenAvailable", argsString, task =
    IO.defer:
      if after < _last then
        IO.True // Event already waiting
      else
        until
          .traverse(_.timeLeftOrZero)
          .flatMap: maybeTimeLeft =>
            if maybeTimeLeft.exists(_.isZero) then
              IO.False // Timeout
            else
              val io = whenAvailable2(after)
                .andWait(delay min maybeTimeLeft.getOrElse(FiniteDuration.MaxValue))
              maybeTimeLeft.fold(io)(t => io.timeoutTo(t, IO.False))

  private def whenAvailable2(after: Long): IO[true] =
    if after < _last then
      trueIO
    else
      synchronized:
        if after < _last then
          trueIO
        else
          val promise = valueToPromise.getOrElseUpdate(after, Promise())
          IO.fromFutureCancelable(IO.pure:
            promise.future ->
              /*onCancel=*/ IO.unit) // TODO Use Deferred instread of Promise to avoid leaks!
            .as(true)

  def last = _last

  @TestOnly
  private[stream] def waitingCount =
    synchronized:
      valueToPromise.size

object IncreasingNumberSync:
  private val trueIO = IO.pure[true](true)
