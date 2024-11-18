package js7.base.utils

import cats.effect.{IO, Outcome, OutcomeIO}
import js7.base.catsutils.CatsDeadline
import js7.base.catsutils.CatsEffectExtensions.guaranteeCaseLazy
import js7.base.log.Logger.syntax.*
import js7.base.log.{LogLevel, Logger}
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.syntax.*
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.base.utils.Worry.*
import scala.concurrent.duration.FiniteDuration
import scala.math.Ordering.Implicits.infixOrderingOps

/** When start to worry about an operation lasting too long. */
final case class Worry(
  durations: Seq[FiniteDuration] = DefaultWorryDurations,
  infoLevel: FiniteDuration = InfoWorryDuration,
  orangeLevel: FiniteDuration = InfoWorryDuration,
  maxLogLevel: LogLevel = LogLevel.MaxValue):

  def logLevel(elapsed: FiniteDuration): LogLevel =
    if elapsed < infoLevel then
      LogLevel.Debug
    else
      LogLevel.Info

  def symbol(elapsed: FiniteDuration): String =
    if elapsed < orangeLevel then "🟡" else "🟠"

  def logWhenItTakesLonger[A](what: => String)(io: IO[A]): IO[A] =
    logWhenItTakesLonger("for", "completed", what):
      io

  def logWhenItTakesLonger[A](io: IO[A])(using enclosing: sourcecode.Enclosing): IO[A] =
    logWhenItTakesLonger("in", "continues", enclosing.value):
      io

  def logWhenItTakesLonger[A](preposition: String, completed: String, what: => String)(io: IO[A])
  : IO[A] =
    logWhenItTakesLonger_(io, maxLogLevel = maxLogLevel):
      case (None, elapsed, level, sym) => IO.pure:
        s"$sym Still waiting $preposition $what for ${elapsed.pretty}"
      case (Some(Outcome.Succeeded(_)), elapsed, level, sym) => IO.pure:
        s"$sym $what $completed after ${elapsed.pretty}"
      case (Some(Outcome.Canceled()), elapsed, level, sym) => IO.pure:
        s"$sym $what canceled after ${elapsed.pretty}"
      case (Some(Outcome.Errored(t)), elapsed, level, sym) => IO.pure:
        s"$sym $what failed after ${elapsed.pretty} with ${t.toStringWithCauses}"

  def logWhenItTakesLonger_[A](io: IO[A], maxLogLevel: LogLevel = LogLevel.MaxValue)
    (onDelayedOrCompleted: (Option[OutcomeIO[A]], FiniteDuration, LogLevel, String) => IO[String])
  : IO[A] =
    CatsDeadline.now.flatMap: since =>
      var level: LogLevel = LogLevel.None
      io.whenItTakesLonger(durations): duration =>
        level = logLevel(duration) min maxLogLevel
        onDelayedOrCompleted(None, duration, level, symbol(duration))
          .flatMap: line =>
            IO(logger.log(level, line))
      .guaranteeCaseLazy: outcome =>
        IO.whenA(level != LogLevel.None):
          since.elapsed.flatMap: elapsed =>
            val sym = outcome match
              case Outcome.Succeeded(_) => "🔵"
              case Outcome.Canceled() => "◼️"
              case Outcome.Errored(t) => "💥"
            onDelayedOrCompleted(Some(outcome), elapsed, level, sym)
              .flatMap: line =>
                IO(logger.log(level, line))


object Worry:

  private val logger = Logger[this.type]

  val AfterTenSecondsWorryDurations: Seq[FiniteDuration] =
    Seq.fill(((1.h - 10.s) / 10.s).toInt)(10.s) :+ 60.s

  val DefaultWorryDurations: Seq[FiniteDuration] =
    Seq(3.s, 7.s) ++ AfterTenSecondsWorryDurations

  private val InfoWorryDuration: FiniteDuration =
    30.s

  val Default: Worry = Worry()

  def adaptDurationsToMinimum(
    minimum: FiniteDuration,
    worryDurations: Seq[FiniteDuration] = DefaultWorryDurations)
  : Seq[FiniteDuration] =
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
