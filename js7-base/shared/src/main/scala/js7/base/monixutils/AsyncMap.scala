package js7.base.monixutils

import cats.effect.ExitCase
import cats.syntax.flatMap._
import js7.base.problem.Checked
import js7.base.problem.Problems.DuplicateKey
import js7.base.utils.ScalaUtils.implicitClass
import js7.base.utils.ScalaUtils.syntax._
import monix.catnap.MVar
import monix.eval.Task
import scala.reflect.ClassTag

final class AsyncMap[K: ClassTag, V](initial: Map[K, V])
{
  private val mvarTask = MVar.of[Task, Map[K, V]](initial).memoize

  def insert(key: K, value: V): Task[Checked[Unit]] =
    updateChecked(key, {
      case None => Task.pure(Right(value))
      case Some(_) => Task.pure(Left(DuplicateKey(implicitClass[K].simpleScalaName, key.toString)))
    }).rightAs(())

  def isEmpty: Task[Boolean] =
    all.map(_.isEmpty)

  def size: Task[Int] =
    all.map(_.size)

  def all: Task[Map[K, V]] =
    mvarTask.flatMap(_.read)

  def removeAll: Task[Map[K, V]] =
    mvarTask.flatMap(mvar => mvar
      .take
      .flatMap(result => mvar.put(Map.empty).as(result)))
      .uncancelable

  def get(key: K): Task[Option[V]] =
    mvarTask.flatMap(_.read)
      .map(_.get(key))

  def checked(key: K): Task[Checked[V]] =
    mvarTask.flatMap(_.read)
      .map(_.checked(key))

  def remove(key: K): Task[Option[V]] =
    mvarTask
      .flatMap(mvar =>
        mvar.take.flatMap { map =>
          val removed = map.get(key)
          mvar.put(map - key).as(removed)
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
          Task
            .defer/*catch inside task*/(update(previous))
            .guaranteeCase {
              case ExitCase.Completed => Task.unit
              case _ => mvar.put(map)
            }
            .flatMap(value =>
              mvar.put(map + (key -> value))
            .as(previous -> value))
        })
      .uncancelable

  def updateChecked(key: K, update: Option[V] => Task[Checked[V]]): Task[Checked[V]] =
    mvarTask
      .flatMap(mvar =>
        mvar.take.flatMap(map =>
          Task
            .defer/*catch inside task*/(update(map.get(key)))
            .guaranteeCase {
              case ExitCase.Completed => Task.unit
              case _ => mvar.put(map)
            }
            .flatTap(checked => mvar.put(checked match {
              case Left(_) => map
              case Right(value) => map + (key -> value)
            }))))
      .uncancelable
}

object AsyncMap
{
  def apply[K: ClassTag, V](initial: Map[K, V] = Map.empty[K, V]) =
    new AsyncMap(initial)

  def empty[K: ClassTag, V] =
    new AsyncMap(Map.empty[K, V])
}
