package js7.base.monixutils

import js7.base.problem.Checked
import js7.base.problem.Problems.{DuplicateKey, UnknownKeyProblem}
import js7.base.utils.ScalaUtils.implicitClass
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.{AsyncLock, LockKeeper}
import monix.eval.Task
import scala.reflect.ClassTag

final class AsyncMap[K: ClassTag, V](initial: Map[K, V])
{
  private val lockKeeper = new LockKeeper[K]
  private val lock = AsyncLock(s"AsyncMap[${implicitClass[K].shortClassName}]")
  @volatile private var _map = initial

  def insert(key: K, value: V): Task[Checked[Unit]] =
    updateChecked(key, {
      case None => Task.pure(Right(value))
      case Some(_) => Task.pure(Left(DuplicateKey(implicitClass[K].simpleScalaName, key.toString)))
    }).rightAs(())

  def isEmpty: Boolean =
    _map.isEmpty

  def size: Int =
    _map.size

  def toMap: Map[K, V] =
    _map

  def contains(key: K): Boolean =
    _map.contains(key)

  def get(key: K): Option[V] =
    _map.get(key)

  def checked(key: K): Checked[V] =
    _map.checked(key)

  /** Not synchronized with other updates! */
  def removeAll: Task[Map[K, V]] =
    lock.lock(Task {
      val result = _map
      _map = Map.empty
      result
    })

  def remove(key: K): Task[Option[V]] =
    lockKeeper.lock(key)(
      lock.lock(Task {
        val result = _map.get(key)
        _map = _map.removed(key)
        result
      }))

  def updateExisting(key: K, update: V => Task[Checked[V]]): Task[Checked[V]] =
    updateChecked(key, {
      case None => Task.pure(Left(UnknownKeyProblem(implicitClass[K].simpleScalaName, key)))
      case Some(existing) => update(existing)
    })

  def getOrElseUpdate(key: K, value: Task[V]): Task[V] =
    update(key, {
      case Some(existing) => Task.pure(existing)
      case None => value
    })

  def update(key: K, update: Option[V] => Task[V]): Task[V] =
    getAndUpdate(key, update)
      .map(_._2)

  def getAndUpdate(key: K, update: Option[V] => Task[V]): Task[(Option[V], V)] =
    lock.lock(Task.defer {
      val previous = _map.get(key)
      update(previous)
        .map { updated =>
          _map += key -> updated
          updated
        }
        .map(previous -> _)
    })

  def updateChecked(key: K, update: Option[V] => Task[Checked[V]]): Task[Checked[V]] =
    lockKeeper.lock(key)(
      Task.defer/*catch inside task*/(update(_map.get(key)))
        .flatMap { updated =>
          updated
            .match_ {
              case Left(p) => Task.pure(Left(p))
              case Right(updated) => lock.lock(Task {
                _map += key -> updated
                Checked.unit
              })
            }
            .as(updated)
        })

  def updateCheckedWithResult[R](key: K, update: Option[V] => Task[Checked[(V, R)]])
  : Task[Checked[R]] =
    lockKeeper.lock(key)(
      Task.defer {
        update(_map.get(key))
          .flatMapT { case (v, r) =>
            lock
              .lock(Task {
                _map += key -> v
              })
              .as(Right(r))
          }
      })
}

object AsyncMap
{
  def apply[K: ClassTag, V](initial: Map[K, V] = Map.empty[K, V]) =
    new AsyncMap(initial)

  def empty[K: ClassTag, V] =
    new AsyncMap(Map.empty[K, V])
}
