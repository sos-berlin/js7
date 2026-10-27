package js7.base.utils

import cats.effect.kernel.Resource.ExitCase
import cats.effect.{IO, Resource, ResourceIO}
import cats.syntax.flatMap.*
import js7.base.catsutils.CatsDeadline
import js7.base.catsutils.CatsEffectExtensions.{defer, right}
import js7.base.log.Logger.syntax.*
import js7.base.log.{LogLevel, Logger}
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.syntax.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.Tests.isStrict
import js7.base.utils.Worry.*
import scala.concurrent.duration.FiniteDuration
import scala.math.Ordering.Implicits.infixOrderingOps

/** When start to worry about an operation lasting too long. */
final case class Worry(
  durations: List[FiniteDuration],
  infoLevel: FiniteDuration,
  orangeLevel: FiniteDuration,
  maxLogLevel: LogLevel):

  private val (debugLevel, myDurations) =
    val traceAdded = logger.isTraceEnabled && durations.headOption.exists(_ > TraceWorryDuration)
    if traceAdded then
      (Some(durations.head), TraceWorryDuration :: (durations.head - TraceWorryDuration) :: durations.tail)
    else
      (None, durations)

  def logLevel(elapsed: FiniteDuration): LogLevel =
    if debugLevel.exists(elapsed < _) then
      LogLevel.Trace
    else if elapsed < infoLevel then
      LogLevel.Debug
    else
      LogLevel.Info

  def symbol(elapsed: FiniteDuration): String =
    if debugLevel.exists(elapsed < _) then
      "⚪️"
    else if elapsed < orangeLevel then
      "🟡"
    else
      "🟠"

  def logWhenMethodTakesLonger(using enclosing: sourcecode.Enclosing): ResourceIO[Unit] =
    logWhenItTakesLonger(enclosing.value)

  def logWhenItTakesLonger(what: => String): ResourceIO[Unit] =
    logWhenItTakesLonger("for", "completed", what)

  def logWhenItTakesLonger(using enclosing: sourcecode.Enclosing): ResourceIO[Unit] =
    logWhenItTakesLonger("in", "continues", enclosing.value)

  /** Logs in the background like `logWhenItTakesLonger` until the resource is released.
    *
    * When the resource is released, the `completed` message is logged (if something has been
    * logged before), or a message about cancellation or failure of the use of the resource.
    * The release waits until this has been done. */
  def logWhenItTakesLonger(preposition: String, completed: String, what: => String)
  : ResourceIO[Unit] =
    logWhenItTakesLonger:
      (maybeOutcome, elapsed, _, sym) =>
        IO.pure:
          message(preposition, completed, what, maybeOutcome, elapsed, sym)

  def logWhenItTakesLonger(
    onDelayedOrCompleted: (Option[ExitCase], FiniteDuration, LogLevel, String) => IO[String])
  : ResourceIO[Unit] =
    Resource.eval(CatsDeadline.now).flatMap: since =>
      val logging = Logging(onDelayedOrCompleted)
      // Resources are released in reverse order:
      // the timer is canceled first, to log nothing more after the final message
      Resource.onFinalizeCase(logging.onFinished(since, _)) >>
        Worry.whenItTakesLonger(myDurations)(logging.onDelayed)

  /** As long as `io` has not completed, call `thenDo` after each of `durations`, without logging.
    *
    * Unlike logging, `thenDo` is not called earlier when trace logging is enabled.
    *
    * @param thenDo A function which gets the elapsed time since start as argument. */
  def whenItTakesLonger(thenDo: FiniteDuration => IO[Unit]): ResourceIO[Unit] =
    Worry.whenItTakesLonger(durations)(thenDo)


  /** Logs "still waiting" messages and the final message.. */
  private final class Logging(
    onDelayedOrCompleted: (Option[ExitCase], FiniteDuration, LogLevel, String) => IO[String]):

    private var stackDumped = !isStrict
    @volatile private var level: LogLevel = LogLevel.None

    def onDelayed(duration: FiniteDuration): IO[Unit] =
      IO.defer:
        level = logLevel(duration) min maxLogLevel
        onDelayedOrCompleted(None, duration, level, symbol(duration))
          .flatMap: line =>
            IO:
              logger.log(level, line)
              logger.whenTraceEnabled: // Allows dynamic switching to trace in log4j2.xml
                // Shorter than 99.s usually used in tests
                if !stackDumped && duration >= 99.s - 9.s then
                  stackDumped = true
                  dumpStacks(line)

    def onFinished(since: CatsDeadline, exitCase: ExitCase): IO[Unit] =
      IO.defer: // Evaluate `level` when the operation has finished, not when this IO is created
        IO.whenA(level != LogLevel.None):
          since.elapsed.flatMap: elapsed =>
            onDelayedOrCompleted(Some(exitCase), elapsed, level, finishedSymbol(exitCase))
              .map: line =>
                logger.log(level, line)

    private def dumpStacks(line: String): Unit =
      // Maybe a worker thread blocks. We show all thread stacks.
      Threads.allThreadStackTraces().foreach(logger.trace)
      //println(line)
      //triggerCatsEffectFiberDump() // EXPERIMENTAL

    //private def triggerCatsEffectFiberDump(): Unit =
    //  if !isWindows then
    //    try
    //      val process = ProcessBuilder("kill", "-USR1", ProcessHandle.current.pid.toString)
    //        .redirectInput(Redirect.PIPE)
    //        .start()
    //      process.getOutputStream.close()
    //      process.waitFor()
    //    catch case _: Exception => ()

  end Logging


object Worry:

  private val logger = Logger[this.type]

  def apply(
    durations: Iterable[FiniteDuration] = DefaultWorryDurations,
    infoLevel: FiniteDuration = InfoWorryDuration,
    orangeLevel: FiniteDuration = InfoWorryDuration,
    maxLogLevel: LogLevel = LogLevel.MaxValue)
  : Worry =
    new Worry(durations.toList, infoLevel, orangeLevel, maxLogLevel)

  /** As long as the resource is in use, call `thenDo` after each of `durations`.
    *
    * @param durations if empty then `thenDo` will not be called.
    *                  The last entry is repeated until the resource is released,
    *                  except a zero duration.
    *                  A zero duration calls `thenDo` immediately.
    *                  A negative duration terminates calling of `thenDo`.
    * @param thenDo    A function which gets the elapsed time since start as argument. */
  def whenItTakesLonger(durations: IterableOnce[FiniteDuration])
    (thenDo: FiniteDuration => IO[Unit])
  : ResourceIO[Unit] =
    Resource.defer:
      val durationIterator = durations.iterator
      if durationIterator.isEmpty then
        Resource.unit
      else
        CatsDeadline.now.flatMap: since =>
          ZeroDuration.tailRecM: lastDuration =>
            val d = durationIterator.nextOption().getOrElse:
              if lastDuration.isZero then
                (-1).s // Avoid hot loop at end of iterator. Instead, terminate the loop
              else
                lastDuration
            if d.isNegative then
              IO.right(())
            else
              IO.sleep(d)
                .*>(since.elapsed)
                .flatMap(thenDo)
                .as(Left(d))
        .background.void

  private def finishedSymbol(exitCase: ExitCase): String =
    exitCase match
      case ExitCase.Succeeded => "🔵"
      case ExitCase.Canceled=> "◼️ "
      case _: ExitCase.Errored => "💥"

  private def message(
    preposition: String,
    completed: String,
    what: => String,
    maybeOutcome: Option[ExitCase],
    elapsed: FiniteDuration,
    sym: String)
  : String =
    maybeOutcome match
      case None =>
        s"$sym Still waiting $preposition $what for ${elapsed.pretty}"
      case Some(ExitCase.Succeeded) =>
        s"$sym $what $completed after ${elapsed.pretty}"
      case Some(ExitCase.Canceled) =>
        s"$sym $what canceled after ${elapsed.pretty}"
      case Some(ExitCase.Errored(t)) =>
        s"$sym $what failed after ${elapsed.pretty} with ${t.toStringWithCauses}"

  val AfterTenSecondsWorryDurations: List[FiniteDuration] =
    List.fill(((1.h - 10.s) / 10.s).toInt)(10.s) :+ 60.s

  val DefaultWorryDurations: List[FiniteDuration] =
    3.s :: 7.s :: AfterTenSecondsWorryDurations

  private val TraceWorryDuration: FiniteDuration =
    1.s

  private val InfoWorryDuration: FiniteDuration =
    30.s

  val Default: Worry = Worry()

  def adaptDurationsToMinimum(
    minimum: FiniteDuration,
    worryDurations: List[FiniteDuration] = DefaultWorryDurations)
  : List[FiniteDuration] =
    val dropped = fs2.Stream.iterable(worryDurations)
      .scan(ZeroDuration)(_ + _)
      .drop(1)
      .takeThrough(_ <= minimum)
      .toVector
    if worryDurations.sizeIs == dropped.size then
      worryDurations.last :: Nil
    else
      val second = dropped.last - minimum
      if minimum <= second then
        minimum +:
          second +:
          worryDurations.drop(dropped.size)
      else
        minimum +:
          (worryDurations(dropped.size) + second) +:
          worryDurations.drop(dropped.size + 1)
