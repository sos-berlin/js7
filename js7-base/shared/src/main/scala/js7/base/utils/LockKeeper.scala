package js7.base.utils

import cats.effect.Resource
import js7.base.monixutils.MonixBase.deferFutureAndLog
import js7.base.utils.LockKeeper._
import js7.base.utils.ScalaUtils.syntax._
import monix.eval.Task
import monix.execution.atomic.AtomicBoolean
import scala.collection.mutable
import scala.concurrent.Promise

// TODO Timeout, web service for inspection ?

final class LockKeeper[K]
{
  // keyMap.contains(key): key is locked
  // keyMap(key).length: Number of clients waiting to get the lock
  private val keyMap = mutable.Map.empty[Any, mutable.Queue[Promise[Token]]]

  def lock[A](key: K)(body: Task[A]): Task[A] =
    lockResource(key).use(_ => body)

  def lockResource(key: K): Resource[Task, Token] =
    Resource.make(acquire(key))(release)

  private def acquire(key: K): Task[Token] =
    Task.defer {
      synchronized {
        keyMap.get(key) match {
          case None =>
            logger.trace(s"Acquired lock '$key'")
            keyMap += key -> mutable.Queue.empty
            Task.pure(new Token(key))

          case Some(queue) =>
            val promise = Promise[Token]()
            queue += promise
            logger.trace(s"Acquire lock '$key', queuing (#${queue.length})")
            deferFutureAndLog(promise.future, s"acquiring lock for $key")
        }
      }
    }

  private def release(token: Token): Task[Unit] =
    Task {
      if (!token.released.getAndSet(true)) {
        synchronized {
          import token.key
          keyMap(key).dequeueFirst(_ => true) match {
            case None =>
              logger.trace(s"Released lock '$key'")
              keyMap.remove(key)
            case Some(promise) =>
              logger.trace(s"Released lock '$key', handing over to queued request")
              promise.success(new Token(key))
          }
        }
      }
    }

  override def toString =
    s"LogKeeper(${
      synchronized {
        (for ((key, queue) <- keyMap) yield
          if (queue.isEmpty) key.toString
          else s"$key (${queue.length} waiting)"
        ).mkString(", ")
      }
    })"

  final class Token private[LockKeeper](private[LockKeeper] val key: K)
  {
    private[LockKeeper] val released = AtomicBoolean(false)

    override def toString = s"LockKeeper.Token($key${released.get() ?? ", released"})"
  }
}

object LockKeeper
{
  private val logger = scribe.Logger[this.type]
}