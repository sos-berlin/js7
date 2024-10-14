package js7.base.utils

import cats.effect.{Async, IO}
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import fs2.Stream
import js7.base.catsutils.CatsDeadline
import js7.base.catsutils.CatsEffectExtensions.*
import js7.base.fs2utils.StreamExtensions.*
import js7.base.log.{BlockingSymbol, Logger}
import js7.base.time.ScalaTime.*
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.Delayer.*
import scala.concurrent.duration.*

// Stateful
final class Delayer[F[_]] private(using F: Async[F])(initialNow: CatsDeadline, conf: DelayConf):

  import conf.{delays, resetWhen}

  private val _state = Atomic(State(initialNow, conf.lazyList))
  private val sym = BlockingSymbol()

  export sym.{logLevel, symbol, relievedLogLevel, used as escalated}

  def reset: F[Unit] =
    F.delay:
      _state := State(initialNow, conf.lazyList)
      sym.clear()

  def sleep: F[Unit] =
    sleep_(_ => F.unit)

  def sleep(onSleep: FiniteDuration => F[Unit]): F[Unit] =
    sleep_(onSleep)

  /** Concurrently callable */
  private def sleep_(onSleep: FiniteDuration => F[Unit]): F[Unit] =
    nextDelay2.flatMap: (elapsed, delay) =>
      logger.trace(s"sleep ${delay.pretty} elapsed=${elapsed.pretty} $toString")
      sym.escalate()
      onSleep(delay) *> F.sleep(delay)

  def nextDelay: F[FiniteDuration] =
    nextDelay2.map(_._2)

  private def nextDelay2: F[(FiniteDuration, FiniteDuration)] =
    F.tailRecM(()): _ =>
      F.monotonic.map(CatsDeadline.fromMonotonic).flatMap: now =>
        F.delay:
          val state = _state.get()
          val (elapsed, delay, next) = _state.get().next(now)
          val ok = _state.compareAndSet(state, next)
          if !ok then Left(()) else Right(elapsed -> delay)

  override def toString =
    s"Delayer(${_state.get()} $conf)"


  private sealed case class State(since: CatsDeadline, nextDelays: LazyList[FiniteDuration]):
    def next(now: CatsDeadline): (FiniteDuration, FiniteDuration, State) =
      val elapsed = now - since
      val delay = (nextDelays.head - elapsed) max ZeroDuration
      val next = State(
        now + delay,
        nextDelays =
          if elapsed >= resetWhen then
            conf.lazyList
          else
            nextDelays.tail)
      (elapsed, delay, next)

    override def toString =
      s"nextDelay=${nextDelays.headOption.getOrElse(delays.last).pretty}"


object Delayer:
  private val logger = Logger[this.type]

  def start[F[_]](conf: DelayConf)(using F: Async[F]): F[Delayer[F]] =
    F.monotonic
      .map(CatsDeadline.fromMonotonic)
      .map(now => new Delayer(now, conf))

  def stream[F[_]](conf: DelayConf)(using F: Async[F]): Stream[F, Unit] =
    Stream
      .eval(start(conf))
      .flatMap(delayer => Stream
        .constant((), chunkSize = 1)
        .evalMap(_ => delayer.sleep))
      .prependOne(())

  object extensions:
    extension[A](io: IO[A])
      def onFailureRestartWithDelayer(
        conf: DelayConf,
        onFailure: Throwable => IO[Unit] = _ => IO.unit,
        onSleep: FiniteDuration => IO[Unit] = _ => IO.unit)
      : IO[A] =
        conf.runIO: delayer =>
          ().tailRecM: _ =>
            io.attempt.flatMap:
              case Left(throwable) =>
                onFailure(throwable) *> delayer.sleep(onSleep).as(Left(()))
              case Right(a) =>
                IO.right(a)
