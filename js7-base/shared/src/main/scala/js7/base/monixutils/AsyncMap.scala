package js7.base.monixutils

import cats.syntax.apply.*
import izumi.reflect.Tag
import js7.base.problem.Problems.{DuplicateKey, UnknownKeyProblem}
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.implicitClass
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{AsyncLock, LockKeeper}
import monix.eval.Task
import scala.concurrent.Promise
import scala.reflect.ClassTag

class AsyncMap[K: Tag: ClassTag, V: Tag](initial: Map[K, V] = Map.empty[K, V])
{
  private val lockKeeper = new LockKeeper[K]
  protected val name =
    s"AsyncMap[${implicitly[Tag[K]].tag}, ${implicitly[Tag[V]].tag}]"
  private val shortLock = AsyncLock(s"$name.shortLock", suppressLog = true)
  @volatile private var _map = initial

  protected[AsyncMap] def onEntryInsert() = Checked.unit
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
    removeConditional(_ => true)

  /** Not synchronized with other updates! */
  final def removeConditional(predicate: ((K, V)) => Boolean)(implicit src: sourcecode.Enclosing)
  : Task[Map[K, V]] =
    shortLock.lock(Task {
      val (removed, remaining) = _map.partition(predicate)
      _map = remaining
      onEntryRemoved()
      removed
    })

  final def remove(key: K)(implicit src: sourcecode.Enclosing): Task[Option[V]] =
    lockKeeper.lock(key)(
      shortLock.lock(Task {
        val removed = _map.get(key)
        _map = _map.removed(key)
        if (removed.isDefined) onEntryRemoved()
        removed
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
        updateMap(key, value).orThrow
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
              updateMap(key, updated).orThrow
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
                  updateMap(key, v)
                })
            }
            .map(_.*>(updated))
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
                updateMap(key, v)
              })
              .rightAs(r)
          }
      })

  private def updateMap(key: K, value: V): Checked[Unit] = {
    val checked = if (_map.contains(key)) Checked.unit else onEntryInsert()
    for (_ <- checked) {
      _map = _map.updated(key, value)
    }
    checked
  }

  override def toString =
    s"$name(n=${_map.size})"
}

object AsyncMap
{
  private val logger = scribe.Logger[this.type]

  def apply[K: Tag: ClassTag, V: Tag](initial: Map[K, V] = Map.empty[K, V]) =
    new AsyncMap(initial)

  def stoppable[K: Tag: ClassTag, V: Tag](initial: Map[K, V] = Map.empty[K, V]) =
    new AsyncMap(initial) with Stoppable

  def empty[K: Tag: ClassTag, V: Tag] =
    new AsyncMap(Map.empty[K, V])

  trait Stoppable {
    this: AsyncMap[?, ?] =>

    @volatile private var stoppingProblem: Problem = null
    private val whenEmptyPromise = Promise[Unit]()

    private def isStopping = stoppingProblem != null

    def isStoppingWith(problem: Problem) =
      problem == stoppingProblem

    final val whenStopped: Task[Unit] =
      Task.fromFuture(whenEmptyPromise.future)
        .*>(Task(logger.debug(s"$name stopped")))
        .memoize

    /** Initiate stop. */
    final val initiateStop: Task[Unit] =
      initiateStopWithProblem(Problem.pure(s"$name is being stopped"))

    final def initiateStopWithProblem(problem: Problem): Task[Unit] =
      Task.defer {
        shortLock
          .lock(Task {
            stoppingProblem = problem
            _map.isEmpty
          })
          .map { isEmpty =>
            if (isEmpty) whenEmptyPromise.success(())
          }
      }

    final val stop: Task[Unit] =
      initiateStop *> whenStopped

    override protected[monixutils] final def onEntryInsert(): Checked[Unit] =
      Option(stoppingProblem).toLeft(())

    override protected[monixutils] final def onEntryRemoved(): Unit =
      if (isStopping && isEmpty) whenEmptyPromise.trySuccess(())
  }
}
