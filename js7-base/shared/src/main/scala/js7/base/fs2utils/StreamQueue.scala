package js7.base.fs2utils

import cats.effect.IO
import cats.effect.std.Mutex
import fs2.concurrent.SignallingRef
import js7.base.fs2utils.StreamQueue.*
import scala.collection.mutable

// Unused !!!
private final class StreamQueue[K, V] private(
  toKey: V => K,
  mutex: Mutex[IO],
  stopSignal: SignallingRef[IO, Boolean],
  availableSignal: SignallingRef[IO, Unit]):

  private type Value = StreamQueue.Value[V]

  private val queue = mutable.ListBuffer.empty[Value | End]
  // isQueued is for optimisation
  private val isQueued = mutable.Set.empty[K]

  def nonEmpty: IO[Boolean] =
    mutex.lock.surround:
      IO:
        queue.nonEmpty

  /** Stop immediately. */
  def stop: IO[Unit] =
    stopSignal.set(true)

  /** Close queue properly. */
  def close: IO[Unit] =
    mutex.lock.surround:
      IO:
        queue += End
    *>
      availableSignal.set(())

  def stream: fs2.Stream[IO, V] =
    fs2.Stream.eval(dequeueNext)
      .repeat
      .takeWhile(_ != End)
      .collect:
        case Value(v) => v
      .interruptWhen(stopSignal)

  private def dequeueNext: IO[Value | End] =
    availableSignal.discrete
      .evalMap: _ =>
        tryDequeueNext
      .collect:
        case o: (Value | End) @unchecked => o
      .head.compile.lastOrError

  private def tryDequeueNext: IO[Value | End | NoValue] =
    mutex.lock.surround:
      IO:
        if queue.isEmpty then
          NoValue
        else
          queue.remove(0)

  def enqueue(value: V): IO[Boolean] =
    mutex.lock.surround:
      IO:
        // race? IO.unlessA(isQueued(toKey(value))):
        queue += Value(value)
        isQueued += toKey(value)
      *>
        availableSignal.set(()).as(true)

  //// Slow
  //def withdraw(key: K): IO[Boolean] =
  //  mutex.lock.surround:
  //    IO:
  //      isQueued.remove(key) && locally:
  //        queue.indexWhere:
  //          case None => false
  //          case Some(v) => toKey(v) == key
  //        match
  //          case -1 => false
  //          case i => queue.remove(i); true

  override def toString = "StreamQueue"

object StreamQueue:

  def apply[K, V](toKey: V => K): IO[StreamQueue[K, V]] =
    for
      mutex <- Mutex[IO]
      availableSignal <- SignallingRef[IO].of(())
      stopSignal <- SignallingRef[IO].of(false)
      queue <- IO(new StreamQueue[K, V](toKey, mutex, stopSignal, availableSignal))
    yield
      queue

  private type NoValue = NoValue.type

  private case object NoValue
  private final case class Value[V](value: V)

  private type End = End.type
  private case object End
