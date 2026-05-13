package js7.base.fs2utils

import cats.Eq
import cats.effect.IO
import fs2.Stream
import js7.base.catsutils.SyncDeadline
import js7.base.fs2utils.StreamExtensions.*
import scala.concurrent.duration.*

object HeartbeatDetector:
  extension[V](stream: Stream[IO, V])
    def detectHeartbeat(
      heartbeat: FiniteDuration,
      late: FiniteDuration,
      expired: FiniteDuration,
      tick: FiniteDuration)
    : Stream[IO, Expirable[V]] =
      Stream.eval(SyncDeadline.now).flatMap: start =>
        val ticking = Tick(start) +:
          Stream.awakeEvery[IO](tick).map: elapsed =>
            Tick(start + elapsed)
        stream
          .chunks.evalMap: chunk =>
            SyncDeadline.now.map: now =>
              chunk.map(LiveValue(_, expectedSince = now + heartbeat))
          .unchunks
          .mergeHaltBoth(ticking)
          .scan1[LiveSignal]:
            case (_, live: LiveValue[V @unchecked]) =>
              live

            case (signal, tick: Tick) =>
              val elapsed = tick.expectedSince - signal.expectedSince
              if elapsed < late then
                Tick(signal.expectedSince)
              else if elapsed < expired then
                Late(signal.expectedSince)
              else
                Expired(signal.expectedSince)

            case (_, Late(_) | Expired(_)) =>
              throw IllegalStateException("HeartbeatDetector")
          .collect:
            case o: Expirable[V @unchecked] => o


  private[fs2utils]  sealed trait LiveSignal:
    private[HeartbeatDetector] def expectedSince: SyncDeadline

  private case class Tick(expectedSince: SyncDeadline) extends LiveSignal

  sealed trait Expirable[V] extends LiveSignal

  object Expirable:
    //given Eq[Expirable[?]] = Eq.instance(_ == _)
    private val eq_ : Eq[Expirable[?]] = Eq.instance(_ == _)
    given [V] => Eq[Expirable[V]] = eq_.asInstanceOf[Eq[Expirable[V]]]

  /** A value has arrived in time. */
  sealed trait Alive[V] extends Expirable[V]:
    def value: V

  object Alive:
    // TODO Option-less unapply?
    def unapply[V](alive: Alive[V]): Some[V] =
      Some(alive.value)

  private[fs2utils] final case class LiveValue[V](value: V, expectedSince: SyncDeadline)
    extends Alive[V]

  /** No sign of life within `late` duration.
    *
    * This is an early warning. */
  final case class Late[V](expectedSince: SyncDeadline) extends Expirable[V]

  /** No sign of life within `expire` duration.*/
  final case class Expired[V](expectedSince: SyncDeadline) extends Expirable[V]
