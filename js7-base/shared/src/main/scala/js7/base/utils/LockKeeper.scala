package js7.base.utils

import cats.effect.{Deferred, IO, Resource, ResourceIO}
import cats.syntax.option.*
import js7.base.log.Logger.syntax.*
import js7.base.log.{BlockingSymbol, CorrelId, LogLevel, Logger}
import js7.base.time.ScalaTime.{RichDeadline, RichDuration}
import js7.base.utils.CatsUtils.syntax.whenItTakesLonger
import js7.base.utils.LockKeeper.*
import js7.base.utils.ScalaUtils.syntax.*
import scala.collection.mutable
import scala.concurrent.duration.Deadline.now


// TODO Timeout, web service for inspection ?

final class LockKeeper[K]:

  // keyToQueue.contains(key): key is locked
  // keyToQueue(key).length: Number of clients waiting for the lock
  private val keyToQueue = mutable.Map.empty[Any, mutable.Queue[Deferred[IO, Token]]]

  def lock[A](key: K)(body: IO[A])(implicit enclosing: sourcecode.Enclosing): IO[A] =
    lockResource(key).surround(body)

  def lockResource(key: K)(implicit enclosing: sourcecode.Enclosing): ResourceIO[Token] =
    Resource.make(acquire(key))(release)

  private def acquire(key: K)(implicit enclosing: sourcecode.Enclosing): IO[Token] =
    IO.defer:
      val since = now
      var waitingLogLevel = none[LogLevel]

      val result = synchronized:
        keyToQueue.get(key) match
          case None =>
            keyToQueue += key -> mutable.Queue.empty
            IO.pure(new Token(key))

          case Some(queue) =>
            waitingLogLevel = LogLevel.Debug.some
            logger.debug(s"🟡 Waiting for $key (in ${enclosing.value})")
            val deferred = Deferred.unsafe[IO, Token]
            queue += deferred
            val sym = new BlockingSymbol
            CorrelId.current.bind(
              deferred.get
                .whenItTakesLonger()(_ => IO {
                  waitingLogLevel = LogLevel.Info.some
                  sym.onInfo()
                  logger.info:
                    s"$sym Still waiting for $key (in ${enclosing.value}) since ${since.elapsed.pretty}"
                }))

      result.flatTap(_ => IO:
        waitingLogLevel.foreach: logLevel =>
          logger.log(logLevel,
            s"🟢 Acquired lock '$key' (${enclosing.value}) after ${since.elapsed.pretty}"))

  private def release(token: Token): IO[Unit] =
    IO.defer:
      IO.unlessA(token.released.getAndSet(true)):
        import token.key
        val dequeued = synchronized:
          val maybe = keyToQueue(key).dequeueFirst(_ => true)
          if maybe.isEmpty then keyToQueue.remove(key)
          maybe
        dequeued.fold(IO.unit): deferred =>
          deferred.complete(new Token(key)).void
        // Log late, but outside the synchronized block
        //if (!handedOver)
        //  logger.trace(s"↙ Released lock '$key'")
        //else
        //  logger.trace(s"↙ Released lock '$key', handed over to queued request")

  override def toString =
    s"LockKeeper(${
      synchronized {
        (for (key, queue) <- keyToQueue yield
          if queue.isEmpty then key.toString
          else s"$key (${queue.length} waiting)"
        ).mkString(", ")
      }
    })"

  final class Token private[LockKeeper](private[LockKeeper] val key: K):
    private[LockKeeper] val released = Atomic(false)

    override def toString = s"LockKeeper.Token($key${released.get() ?? ", released"})"


object LockKeeper:
  private val logger = Logger[this.type]
