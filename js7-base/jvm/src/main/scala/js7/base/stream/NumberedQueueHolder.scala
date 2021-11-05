package js7.base.stream

import izumi.reflect.Tag
import js7.base.monixutils.AsyncMap
import js7.base.problem.Checked
import monix.eval.Task
import monix.reactive.Observable
import scala.reflect.ClassTag

final class NumberedQueueHolder[K: ClassTag, V: Tag]
{
  private val subagentToQueue = AsyncMap.empty[K, ObservableNumberedQueue[V]]

  def addKey(key: K): Task[Unit] =
    subagentToQueue
      .insert(key, new ObservableNumberedQueue)
      .void

  def removeKey(key: K): Task[Unit] =
    subagentToQueue
      .remove(key)
      .void

  def enqueue(key: K, command: V): Task[Checked[Unit]] =
    subagentToQueue
      .checked(key)
      .map(_.map(_
        .enqueue(command)))

  def observable(key: K, after: Long): Task[Checked[Observable[Numbered[V]]]] =
    subagentToQueue
      .checked(key)
      .map(_.flatMap(_.observable(after)))

  def releaseUntil(key: K, after: Long): Task[Checked[Unit]] =
    subagentToQueue
      .checked(key)
      .map(_.flatMap(_
        .releaseUntil(after)))
}
