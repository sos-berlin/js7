package js7.base.monixutils

import cats.effect.concurrent.Deferred
import cats.syntax.apply.*
import izumi.reflect.Tag
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.problem.Problems.{DuplicateKey, UnknownKeyProblem}
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{AsyncLock, LockKeeper}
import monix.eval.Task

class AsyncMap[K: Tag, V: Tag](initial: Map[K, V] = Map.empty[K, V])
{
  private val lockKeeper = new LockKeeper[K]
  protected val name =
    s"AsyncMap[${implicitly[Tag[K]].tag}, ${implicitly[Tag[V]].tag}]"
  protected final val shortLock = AsyncLock(s"$name.shortLock", suppressLog = true)
  @volatile private var _map = initial

  protected[AsyncMap] def onEntryInsert(): Checked[Unit] =
    Checked.unit

  protected[AsyncMap] def onEntryRemoved(): Task[Unit] =
    Task.unit

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
      case Some(_) => Task.pure(Left(DuplicateKey(implicitly[Tag[K]].tag.shortName, key.toString)))
    }).rightAs(value)

  /** Not synchronized with other updates! */
  final def removeAll(implicit src: sourcecode.Enclosing): Task[Map[K, V]] =
    removeConditional(_ => true)

  /** Not synchronized with other updates! */
  final def removeConditional(predicate: ((K, V)) => Boolean)(implicit src: sourcecode.Enclosing)
  : Task[Map[K, V]] =
    shortLock.lock(Task.defer {
      val (removed, remaining) = _map.partition(predicate)
      _map = remaining
      onEntryRemoved()
        .as(removed)
    })

  final def remove(key: K)(implicit src: sourcecode.Enclosing): Task[Option[V]] =
    lockKeeper.lock(key)(
      shortLock.lock(Task.defer {
        val removed = _map.get(key)
        _map = _map.removed(key)
        Task.when(removed.isDefined)(onEntryRemoved())
          .as(removed)
      }))

  final def updateExisting(key: K, update: V => Task[Checked[V]])
    (implicit src: sourcecode.Enclosing)
  : Task[Checked[V]] =
    updateChecked(key, {
      case None => Task.pure(Left(UnknownKeyProblem(implicitly[Tag[K]].tag.shortName, key)))
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
  private val logger = Logger[this.type]

  def apply[K: Tag, V: Tag](initial: Map[K, V] = Map.empty[K, V]) =
    new AsyncMap(initial)

  def stoppable[K: Tag, V: Tag](initial: Map[K, V] = Map.empty[K, V]) =
    new AsyncMap(initial) with Stoppable

  def empty[K: Tag, V: Tag] =
    new AsyncMap(Map.empty[K, V])

  trait Stoppable {
    this: AsyncMap[?, ?] =>

    private val whenEmpty = Deferred.unsafe[Task, Unit]
    @volatile private var stoppingProblem: Problem = null

    private def isStopping = stoppingProblem != null

    def isStoppingWith(problem: Problem) =
      problem == stoppingProblem

    final val whenStopped: Task[Unit] =
      whenEmpty.get
        .*>(Task(logger.debug(s"$name stopped")))
        .memoize

    final val stop: Task[Unit] =
      logger
        .traceTask(s"$name.stop")(
          initiateStop *> whenStopped)
        .memoize

    /** Initiate stop. */
    final def initiateStop: Task[Unit] =
      initiateStopWithProblem(Problem.pure(s"$name is being stopped"))

    final def initiateStopWithProblem(problem: Problem): Task[Unit] =
      Task.defer {
        logger.trace(s"$name initiateStopWithProblem $problem")
        shortLock
          .lock(Task {
            stoppingProblem = problem
            isEmpty
          })
          .flatMap(Task.when(_)(
            whenEmpty.complete(())))
      }

    override protected[monixutils] final def onEntryInsert(): Checked[Unit] =
      Option(stoppingProblem).toLeft(())

    override protected[monixutils] final def onEntryRemoved() =
      Task.defer(
        Task.when(isStopping && isEmpty)(
          whenEmpty.complete(())
            .attempt.void))
  }
}
