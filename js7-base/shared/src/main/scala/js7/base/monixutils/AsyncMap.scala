package js7.base.monixutils

import js7.base.problem.Checked
import js7.base.problem.Problems.{DuplicateKey, UnknownKeyProblem}
import js7.base.utils.ScalaUtils.implicitClass
import js7.base.utils.ScalaUtils.syntax._
import js7.base.utils.{AsyncLock, LockKeeper}
import monix.eval.Task
import scala.concurrent.Promise
import scala.reflect.ClassTag

class AsyncMap[K: ClassTag, V](initial: Map[K, V] = Map.empty[K, V])
{
  private val lockKeeper = new LockKeeper[K]
  private val shortLock = AsyncLock(s"AsyncMap[${implicitClass[K].shortClassName}].shortLock",
    suppressLog = true)
  @volatile private var _map = initial

  protected[AsyncMap] def onEntryRemoved() = {}

  final def isEmpty: Boolean =
    _map.isEmpty

  final def size: Int =
    _map.size

  final def toMap: Map[K, V] =
    _map

  final def contains(key: K): Boolean =
    _map.contains(key)

  final def get(key: K): Option[V] =
    _map.get(key)

  final def checked(key: K): Checked[V] =
    _map.checked(key)

  final def insert(key: K, value: V)(implicit src: sourcecode.Enclosing): Task[Checked[V]] =
    updateChecked(key, {
      case None => Task.pure(Right(value))
      case Some(_) => Task.pure(Left(DuplicateKey(implicitClass[K].simpleScalaName, key.toString)))
    }).rightAs(value)

  /** Not synchronized with other updates! */
  final def removeAll(implicit src: sourcecode.Enclosing): Task[Map[K, V]] =
    shortLock.lock(Task {
      val result = _map
      _map = Map.empty
      result
    })

  final def remove(key: K)(implicit src: sourcecode.Enclosing): Task[Option[V]] =
    lockKeeper.lock(key)(
      shortLock.lock(Task {
        val result = _map.get(key)
        _map = _map.removed(key)
        onEntryRemoved()
        result
      }))

  final def updateExisting(key: K, update: V => Task[Checked[V]])
    (implicit src: sourcecode.Enclosing)
  : Task[Checked[V]] =
    updateChecked(key, {
      case None => Task.pure(Left(UnknownKeyProblem(implicitClass[K].simpleScalaName, key)))
      case Some(existing) => update(existing)
    })

  final def getOrElseUpdate(key: K, value: Task[V])
    (implicit src: sourcecode.Enclosing)
  : Task[V] =
    update(key, {
      case Some(existing) => Task.pure(existing)
      case None => value
    })

  final def put(key: K, value: V)
    (implicit src: sourcecode.Enclosing)
  : Task[V] =
    lockKeeper.lock(key)(
      shortLock.lock(Task {
        _map = _map.updated(key, value)
        value
      }))

  final def update(key: K, update: Option[V] => Task[V])
    (implicit src: sourcecode.Enclosing)
  : Task[V] =
    getAndUpdate(key, update)
      .map(_._2)

  final def getAndUpdate(key: K, update: Option[V] => Task[V])
    (implicit src: sourcecode.Enclosing)
  : Task[(Option[V], V)] =
    lockKeeper.lock(key)(
      Task.defer {
        val previous = _map.get(key)
        update(previous)
          .flatMap(updated =>
            shortLock.lock(Task {
              _map = _map.updated(key, updated)
              updated
          }))
          .map(previous -> _)
      })

  final def updateChecked(key: K, update: Option[V] => Task[Checked[V]])
    (implicit src: sourcecode.Enclosing)
  : Task[Checked[V]] =
    lockKeeper.lock(key)(
      Task.defer/*catch inside task*/(update(_map.get(key)))
        .flatMap { updated =>
          updated
            .match_ {
              case Left(p) => Task.pure(Left(p))
              case Right(v) =>
                shortLock.lock(Task {
                  _map = _map.updated(key, v)
                  Checked.unit
                })
            }
            .as(updated)
        })

  final def updateCheckedWithResult[R](key: K, update: Option[V] => Task[Checked[(V, R)]])
    (implicit src: sourcecode.Enclosing)
  : Task[Checked[R]] =
    lockKeeper.lock(key)(
      Task.defer {
        update(_map.get(key))
          .flatMapT { case (v, r) =>
            shortLock
              .lock(Task {
                _map = _map.updated(key, v)
              })
              .as(Right(r))
          }
      })

  override def toString =
    s"AsyncMap[${implicitClass[K].simpleScalaName}, _](n=${_map.size})"
}

object AsyncMap
{
  def apply[K: ClassTag, V](initial: Map[K, V] = Map.empty[K, V]) =
    new AsyncMap(initial)

  def empty[K: ClassTag, V] =
    new AsyncMap(Map.empty[K, V])

  trait WhenEmpty {
    this: AsyncMap[_, _] =>

    private val whenEmptyPromise = Promise[Unit]()

    final val whenEmpty: Task[Unit] =
      Task
        .defer {
          (if (_map.isEmpty)
            Task.unit
          else {
            Task.fromFuture(whenEmptyPromise.future)
          })
        }
        .memoize

    override protected[monixutils] final def onEntryRemoved(): Unit =
      if (_map.isEmpty) {
        whenEmptyPromise.trySuccess(())
      }
  }
}
