package js7.base.monixutils

import cats.Eq
import cats.effect.IO
import cats.kernel.Eq
import fs2.Stream
import js7.base.catsutils.SyncDeadline
import js7.base.fs2utils.StreamExtensions.*
import scala.concurrent.duration.*

object PauseDetector:

  extension[V](stream: Stream[IO, V])

    // SMALL IMPROVEMENT:
    // Left should contain the timestamp of the last V, not the first Tick after the last V

    def detectPauses(expired: FiniteDuration, late: FiniteDuration, tick: FiniteDuration)
    : Stream[IO, Expirable[V]] =
      Stream.eval(SyncDeadline.now).flatMap: start =>
        val ticking = Tick(start) +:
          Stream.awakeEvery[IO](tick).map: elapsed =>
            Tick(start + elapsed)
        stream
          .map(InTime(_))
          .mergeHaltBoth(ticking)
          .scan1[Element]:
            case (v: (Tick | Late[V @unchecked]), Tick(b)) =>
              val elapsed = b - v.since
              if elapsed < late then
                Tick(v.since)
              if elapsed < expired then
                Late(v.since)
              else
                Expired(v.since)

            case (Expired(a), Tick(_)) =>
              Expired(a)

            case (_, tick: Tick) =>
              tick

            case (_, inTime: InTime[V @unchecked]) =>
              inTime

            case (_, Late(_) | Expired(_)) =>
              throw IllegalStateException("Unexpected Late or Expired in PauseDetector")
          .collect:
            case o: Expirable[V @unchecked] => o

  sealed transparent trait Element

  sealed trait Ticking extends Element

  sealed trait Expirable[V] extends Element

  object Expirable:
    //given Eq[Expirable[?]] = Eq.instance(_ == _)
    private val eq_ : Eq[Expirable[?]] = Eq.instance(_ == _)
    given [V] => Eq[Expirable[V]] = eq_.asInstanceOf[Eq[Expirable[V]]]

  sealed trait Timed extends Element:
    def since: SyncDeadline

  private case class Tick(since: SyncDeadline) extends Ticking, Timed

  /** Late but not expired. */
  final case class Late[V](since: SyncDeadline) extends Expirable[V], Timed

  final case class Expired[V](since: SyncDeadline) extends Expirable[V], Timed

  final case class InTime[V](value: V) extends Ticking, Expirable[V], Element
