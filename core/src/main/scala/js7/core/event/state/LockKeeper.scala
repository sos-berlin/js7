package js7.core.event.state

import cats.effect.Resource
import js7.common.scalautil.Logger
import js7.core.event.state.LockKeeper._
import monix.eval.Task
import monix.execution.atomic.AtomicBoolean
import scala.collection.mutable
import scala.concurrent.Promise

// TODO Timeout, web service for inspection ?

private[state] final class LockKeeper[K]
{
  private val keyMap = mutable.Map[Any, mutable.Queue[Promise[Token]]]()

  def lock(key: K): Resource[Task, Token] =
    Resource.make(acquire(key))(release)

  private def acquire(key: K): Task[Token] =
    Task {
      synchronized {
        keyMap.get(key) match {
          case None =>
            logger.trace(s"acquire '$key', is available")
            val token = new Token(key)
            keyMap += key -> mutable.Queue.empty
            Task.pure(token)

          case Some(queue) =>
            val promise = Promise[Token]()
            queue += promise
            logger.trace(s"acquire '$key', queuing (#${queue.length})")
            Task.fromFuture(promise.future)
        }
      }
    }.flatten

  private def release(token: Token): Task[Unit] =
    Task {
      if (!token.released.getAndSet(true)) {
        synchronized {
          import token.key
          val queue = keyMap(key)
          if (queue.nonEmpty) {
            logger.trace(s"release '$key', handing over to queued request")
            queue.dequeue().success(new Token(key))
          } else {
            logger.trace(s"release '$key'")
            keyMap.remove(key)
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

  private case class GlobalEntry(key: Any, promise: Promise[Token])

  final class Token private[LockKeeper](private[LockKeeper] val key: Any)
  {
    private[LockKeeper] val released = AtomicBoolean(false)

    override def toString = s"LockKeeper.Token($key${if (released.get) ", released" else ""})"
  }
}

private[state] object LockKeeper
{
  private val logger = Logger(getClass)
}
