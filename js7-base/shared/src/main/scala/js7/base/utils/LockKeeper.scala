package js7.base.utils

import cats.effect.{IO, Resource}
import js7.base.log.{BlockingSymbol, CorrelId, Logger}
import js7.base.time.ScalaTime.{RichDeadline, RichDuration}
import js7.base.utils.CatsUtils.syntax.whenItTakesLonger
import js7.base.utils.LockKeeper.*
import js7.base.utils.ScalaUtils.syntax.*
import scala.collection.mutable
import scala.concurrent.Promise
import scala.concurrent.duration.Deadline.now

// TODO Timeout, web service for inspection ?

final class LockKeeper[K]:

  // keyToQueue.contains(key): key is locked
  // keyToQueue(key).length: Number of clients waiting to get the lock
  private val keyToQueue = mutable.Map.empty[Any, mutable.Queue[Promise[Token]]]

  def lock[A](key: K)(body: IO[A])(implicit enclosing: sourcecode.Enclosing): IO[A] =
    lockResource(key).surround(body)

  def lockResource(key: K)(implicit enclosing: sourcecode.Enclosing): Resource[IO, Token] =
    Resource.make(acquire(key))(release)

  private def acquire(key: K)(implicit enclosing: sourcecode.Enclosing): IO[Token] =
    IO.defer:
      var wasQueued, info = false
      val result = synchronized:
        keyToQueue.get(key) match
          case None =>
            keyToQueue += key -> mutable.Queue.empty
            IO.pure(new Token(key))

          case Some(queue) =>
            val since = now
            wasQueued = true
            val promise = Promise[Token]()
            queue += promise
            val sym = new BlockingSymbol
            logger.debug(s"🟡 Waiting for $key (in ${enclosing.value})")
            CorrelId.current.bind(
              IO.fromFuture(IO.pure(promise.future))
                .whenItTakesLonger()(_ => IO {
                  sym.onInfo()
                  info = true
                  logger.info(s"$sym Still waiting for $key (in ${enclosing.value}) since ${since.elapsed.pretty}")
                }))
      if info then
        logger.info(s"🟢 Acquired lock '$key' (${enclosing.value})")
      else {
        //logger.trace(s"↙ Acquired lock '$key' (${enclosing.value})")
      }
      result

  private def release(token: Token): IO[Unit] =
    IO:
      if !token.released.getAndSet(true) then
        import token.key
        val handedOver = synchronized:
          keyToQueue(key).dequeueFirst(_ => true) match
            case None =>
              keyToQueue.remove(key)
              false
            case Some(promise) =>
              promise.success(new Token(key))
              true
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
