package js7.base.monixutils

import cats.effect.IO
import fs2.Stream
import js7.base.catsutils.SyncDeadline
import js7.base.fs2utils.StreamExtensions.*
import scala.concurrent.duration.*

object StreamPauseDetector:

  extension[A](stream: Stream[IO, A])

    // SMALL IMPROVEMENT:
    // Left should contain the timestamp of the last A, not the first Tick after the last A
    /** Returns Right[A], or Left for each pause (only one Left per pause). */
    def detectPauses(delay: FiniteDuration): Stream[IO, Either[SyncDeadline, A]] =
      detectPauses(delay, Left(_), Right(_))

    def detectPauses[P](delay: FiniteDuration, pause: P): Stream[IO, A | P] =
      detectPauses(delay, _ => pause, identity)

    def detectPauses[B](delay: FiniteDuration, fromPause: SyncDeadline => B, fromValue: A => B)
    : Stream[IO, B] =
      detectPauses_(delay).collect:
        case Expired(since) => fromPause(since)
        case Value(a: A @unchecked) => fromValue(a)

    def detectPauses_(delay: FiniteDuration): Stream[IO, Expirable[A]] =
      Stream.eval(SyncDeadline.now).flatMap: start =>
        val ticking = Tick(start) +:
          Stream.awakeEvery[IO](delay).map(elapsed => Tick(start + elapsed))
        stream
          .map(Value(_))
          .mergeHaltBoth(ticking)
          .scan1[Element]:
            case (Tick(o), Tick(_)) => Expired(o)
            case (Expired(o), Tick(_)) => Expired(o)
            case (_, tick: Tick) => tick
            case (_, value: Value[A @unchecked]) => value
            case (_, Expired(_)) => throw new IllegalStateException(
              "Duplicate Expired in StreamPauseDetector")
          .collect:
            case o: Expirable[A @unchecked] => o

  sealed transparent trait Element
  sealed trait Ticking extends Element
  sealed trait Expirable[A] extends Element

  private case class Tick(since: SyncDeadline) extends Ticking
  final case class Expired(since: SyncDeadline) extends Expirable[Nothing]
  final case class Value[A](value: A) extends Ticking, Expirable[A], Element
