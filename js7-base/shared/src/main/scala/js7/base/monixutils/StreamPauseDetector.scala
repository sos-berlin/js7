package js7.base.monixutils

import js7.base.monixutils.MonixBase.syntax.*
import js7.base.monixutils.MonixDeadline.now
import cats.effect.IO
import monix.reactive.subjects.PublishSubject
import scala.concurrent.duration.*
import fs2.Stream

object StreamPauseDetector:

  private implicit val overflowStrategy: OverflowStrategy.BackPressure =
    OverflowStrategy.BackPressure(bufferSize = 2/*minimum*/)

  implicit final class RichPauseStream[A](private val underlying: Stream[IO, A]) extends AnyVal:
    // TODO Left should contain the timestamp of the last A, not the first Tick after the last A
    // TODO May parameterize the current Either return type with two functions for less allocs
    /** Returns Right[A], or Left for each pause (only one Left per pause). */
    def detectPauses(delay: FiniteDuration): Stream[IO, Either[MonixDeadline, A]] =
      detectPauses(delay, Left(_), Right(_))

    def detectPauses[A1 <: A](delay: FiniteDuration, pause: A1)
    : Stream[IO, A] =
      detectPauses(delay, _ => pause, identity)

    def detectPauses[B](delay: FiniteDuration, fromPause: MonixDeadline => B, fromData: A => B)
    : Stream[IO, B] =
      Stream.deferAction(implicit scheduler =>
        Stream[IO, Stream[IO, Ticking]](
          underlying map Data.apply,
          Stream.intervalWithFixedDelay(delay, delay).map(_ => Tick(now))
        ).merge
          .scan[Element[A]](Tick(now)) {
            case (Tick(o), Tick(_)) => Expired(o)
            case (Expired(o), Tick(_)) => Expired(o)
            case (_, tick: Tick) => tick
            case (_, data: Data[A @unchecked]) => data
          }
          .collect {
            case Expired(since) => fromPause(since)
            case Data(a: A @unchecked) => fromData(a)
          })

    // TODO These functions should replace detectPauses2, but check Active/PassiveClusterNode !
    // TODO Left should contain the timestamp of the last A, not the first Tick after the last A
    // TODO May parameterize the current Either return type with two functions for less allocs
    /** Returns Right[A], or Left for each pause (only one Left per pause). */
    def detectPauses2(delay: FiniteDuration): Stream[IO, Either[MonixDeadline, A]] =
      detectPauses2(delay, Left(_), Right(_))

    /** NEW TERMINATING IMPLEMENTATION. */
    def detectPauses2[A1 <: A](delay: FiniteDuration, pause: A1)
    : Stream[IO, A] =
      detectPauses2(delay, _ => pause, identity)

    /** NEW TERMINATING IMPLEMENTATION. */
    def detectPauses2[B](delay: FiniteDuration, fromPause: MonixDeadline => B, fromData: A => B)
    : Stream[IO, B] =
      Stream.deferAction { implicit scheduler =>
        val stop = PublishSubject[Unit]()
        Stream[IO, Stream[IO, Ticking]](
          underlying
            .guaranteeCase(_ => IO(stop.onComplete()))
            .map(Data.apply),
          Stream
            .intervalWithFixedDelay(delay, delay)
            .takeUntil(stop)
            .map(_ => Tick(now))
        ).merge
          .scan[Element[A]](Tick(now)):
            case (Tick(o), Tick(_)) => Expired(o)
            case (Expired(o), Tick(_)) => Expired(o)
            case (_, tick: Tick) => tick
            case (_, data: Data[A @unchecked]) => data
          .collect:
            case Expired(since) => fromPause(since)
            case Data(a: A @unchecked) => fromData(a)
      }

  private sealed trait Element[+A]
  private sealed trait Ticking extends Element[Nothing], Product
  private sealed trait Expirable extends Element[Nothing]

  private case class Tick(since: MonixDeadline) extends Ticking
  private case class Expired(since: MonixDeadline) extends Expirable
  private final case class Data[A](event: A) extends Ticking, Expirable, Element[A]
