package js7.base.utils

import cats.data.NonEmptySeq
import cats.effect.{Async, Timer}
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import js7.base.log.Logger
import js7.base.time.ScalaTime.*
import js7.base.utils.CatsUtils.syntax.*
import js7.base.utils.Delayer.*
import monix.eval.{Task, TaskLike}
import monix.execution.atomic.Atomic
import monix.reactive.Observable
import scala.concurrent.duration.*

// Stateful
final class Delayer[F[_]] private(initialNow: Deadline, conf: DelayConf)
  (implicit F: Async[F], timer: Timer[F])
{
  import conf.{delays, resetWhen}

  private val _state = Atomic(State(initialNow, resetDelays(delays)))

  def sleep: F[Unit] =
    sleep_(_ => F.unit)

  def sleep(onSleep: FiniteDuration => F[Unit]): F[Unit] =
    sleep_(onSleep)

  /** Concurrently callable */
  private def sleep_(onSleep: FiniteDuration => F[Unit]): F[Unit] =
    nextDelay2.flatMap { case (elapsed, delay) =>
      logger.trace(s"sleep ${delay.pretty} elapsed=${elapsed.pretty} $toString")
      onSleep(delay) *> timer.sleep(delay)
    }

  def nextDelay: F[FiniteDuration] =
    nextDelay2.map(_._2)

  private def nextDelay2: F[(FiniteDuration, FiniteDuration)] =
    F.tailRecM(())(_ =>
      timer.now.flatMap(now => F.delay {
        val state = _state.get()
        val (elapsed, delay, next) = _state.get().next(now)
        val ok = _state.compareAndSet(state, next)
        if (!ok) Left(()) else Right(elapsed -> delay)
      }))

  private sealed case class State(since: Deadline, nextDelays: LazyList[FiniteDuration]) {
    def next(now: Deadline): (FiniteDuration, FiniteDuration, State) = {
      val elapsed = now - since
      val delay = (nextDelays.head - elapsed) max ZeroDuration
      val next = State(
        now + delay,
        nextDelays =
          if (elapsed >= resetWhen)
            resetDelays(delays)
          else if (nextDelays.tail.nonEmpty)
            nextDelays.tail
          else
            nextDelays)
      (elapsed, delay, next)
    }

    override def toString =
      s"since=${since.elapsed.pretty} nextDelay=${nextDelays.headOption.getOrElse(delays.last).pretty}"
  }

  override def toString = s"Delayer(${_state.get()} $conf)"
}

object Delayer
{
  private val logger = Logger[this.type]

  def start[F[_]](conf: DelayConf)(implicit F: Async[F], timer: Timer[F])
  : F[Delayer[F]] =
    for (now <- timer.now) yield
      new Delayer(now, conf)

  def observable[F[_]](conf: DelayConf)
    (implicit F: Async[F], timer: Timer[F], taskLike: TaskLike[F])
  : Observable[Unit] =
    Observable
      .from(start(conf))
      .flatMap(delayer => Observable
        .fromIteratorF(F.delay(Iterator.continually(())))
        .mapEvalF(_ => delayer.sleep)
        .prepend(()))


  object syntax {
    implicit final class RichDelayerTask[A](val underlying: Task[A]) extends AnyVal {
      def onFailureRestartWithDelayer(
        conf: DelayConf,
        onFailure: Throwable => Task[Unit] = _ => Task.unit,
        onSleep: FiniteDuration => Task[Unit] = _ => Task.unit)
      : Task[A] =
        Delayer.start[Task](conf).flatMap(delayer =>
          underlying
            .onErrorRestartLoop(()) {
              case (throwable, _, retry) =>
                onFailure(throwable) *>
                  delayer.sleep(onSleep).*>(retry(()))
            })
    }
  }

  private def resetDelays(delays: NonEmptySeq[FiniteDuration]): LazyList[FiniteDuration] =
    LazyList.from(delays.toSeq)
}
