package js7.base.monixutils

import cats.syntax.flatMap._
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax._
import monix.catnap.MVar
import monix.eval.Task
import scala.reflect.ClassTag

final class AsyncMap[K: ClassTag, V](initial: Map[K, V])
{
  private val mvarTask = MVar.of[Task, Map[K, V]](initial).memoize

  def all: Task[Map[K, V]] =
    mvarTask.flatMap(_.read)

  def removeAll: Task[Map[K, V]] =
    mvarTask.flatMap(mvar => mvar
      .take
      .flatMap(result => mvar.put(Map.empty).as(result)))

  def get(key: K): Task[Checked[V]] =
    mvarTask.flatMap(_.read)
      .map(_.checked(key))

  def remove(key: K): Task[Option[V]] =
    mvarTask
      .flatMap(mvar =>
        mvar.take.flatMap { map =>
          val removed = map.get(key)
          mvar.put(map - key).map(_ => removed)
        })
      .uncancelable

  def update(key: K, update: Option[V] => Task[V]): Task[V] =
    getAndUpdate(key, update)
      .map(_._2)

  def getAndUpdate(key: K, update: Option[V] => Task[V]): Task[(Option[V], V)] =
    mvarTask
      .flatMap(mvar =>
        mvar.take.flatMap { map =>
          val previous = map.get(key)
          update(previous)
            .flatMap(value =>
              mvar.put(map + (key -> value))
            .map(_ => previous -> value))
          // TODO Handle failure and cancellation
        })
      .uncancelable

  def updateChecked(key: K, update: Option[V] => Task[Checked[V]]): Task[Checked[V]] =
    mvarTask
      .flatMap(mvar =>
        mvar.take.flatMap(map =>
          update(map.get(key))
            .flatTap(checked => mvar.put(checked match {
              case Left(_) => map
              case Right(value) => map + (key -> value)
              // TODO Handle failure and cancellation
            }))))
      .uncancelable
}

object AsyncMap
{
  def apply[K: ClassTag, V](initial: Map[K, V] = Map.empty) =
    new AsyncMap(initial)
}
