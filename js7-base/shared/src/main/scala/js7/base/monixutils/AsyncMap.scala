package js7.base.monixutils

import cats.effect.{Deferred, IO}
import cats.syntax.apply.*
import izumi.reflect.Tag
import js7.base.catsutils.CatsEffectExtensions.{left, right}
import js7.base.catsutils.UnsafeMemoizable.memoize
import js7.base.log.Logger
import js7.base.log.Logger.syntax.*
import js7.base.problem.Problems.{DuplicateKey, UnknownKeyProblem}
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{AsyncLock, LockKeeper}

class AsyncMap[K: Tag, V: Tag](initial: Map[K, V] = Map.empty[K, V]):

  private val lockKeeper = new LockKeeper[K]
  protected val name =
    s"AsyncMap[${implicitly[Tag[K]].tag}, ${implicitly[Tag[V]].tag}]"
  protected final val shortLock = AsyncLock(s"$name.shortLock", suppressLog = true)
  @volatile private var _map = initial

  protected[AsyncMap] def onEntryInsert(): Checked[Unit] =
    Checked.unit

  protected[AsyncMap] def onEntryRemoved(): IO[Unit] =
    IO.unit

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

  final def insert(key: K, value: V)(using sourcecode.Enclosing): IO[Checked[V]] =
    updateChecked(key):
      case None => IO.right(value)
      case Some(_) => IO.left(DuplicateKey(implicitly[Tag[K]].tag.shortName, key.toString))
    .rightAs(value)

  /** Not synchronized with other updates! */
  final def removeAll(using sourcecode.Enclosing): IO[Map[K, V]] =
    removeConditional(_ => true)

  /** Not synchronized with other updates! */
  final def removeConditional(predicate: ((K, V)) => Boolean)(using sourcecode.Enclosing)
  : IO[Map[K, V]] =
    shortLock.lock(IO.defer {
      val (removed, remaining) = _map.partition(predicate)
      _map = remaining
      onEntryRemoved()
        .as(removed)
    })

  final def remove(key: K)(using sourcecode.Enclosing): IO[Option[V]] =
    lockKeeper.lock(key):
      shortLock.lock:
        IO.defer:
          val removed = _map.get(key)
          _map = _map.removed(key)
          IO.whenA(removed.isDefined)(onEntryRemoved())
            .as(removed)

  final def updateExisting(key: K, update: V => IO[Checked[V]])
    (using sourcecode.Enclosing)
  : IO[Checked[V]] =
    updateChecked(key):
      case None => IO.left(UnknownKeyProblem(implicitly[Tag[K]].tag.shortName, key))
      case Some(existing) => update(existing)

  final def getOrElseUpdate(key: K, value: => IO[V])
    (using sourcecode.Enclosing)
  : IO[V] =
    update(key):
      case Some(existing) => IO.pure(existing)
      case None => value

  final def put(key: K, value: V)
    (using sourcecode.Enclosing)
  : IO[V] =
    lockKeeper.lock(key):
      shortLock.lock:
        IO:
          updateMap(key, value).orThrow
          value

  final def update(key: K)(update: Option[V] => IO[V])
    (using sourcecode.Enclosing)
  : IO[V] =
    getAndUpdate(key, update)
      .map(_._2)

  final def getAndUpdate(key: K, update: Option[V] => IO[V])
    (using sourcecode.Enclosing)
  : IO[(Option[V], V)] =
    lockKeeper.lock(key):
      IO.defer:
        val previous = _map.get(key)
        update(previous)
          .flatMap: updated =>
            shortLock.lock(IO:
              updateMap(key, updated).orThrow
              updated)
          .map(previous -> _)

  final def updateChecked(key: K)(update: Option[V] => IO[Checked[V]])
    (using sourcecode.Enclosing)
  : IO[Checked[V]] =
    lockKeeper.lock(key):
      IO.defer/*catch inside io*/(update(_map.get(key)))
        .flatMap: updated =>
          updated
            .match
              case Left(p) => IO.left(p)
              case Right(v) =>
                shortLock.lock(IO:
                  updateMap(key, v))
            .map(_.*>(updated))

  final def updateCheckedWithResult[R](key: K, update: Option[V] => IO[Checked[(V, R)]])
    (using sourcecode.Enclosing)
  : IO[Checked[R]] = IO.defer:
    lockKeeper.lock(key):
      IO.defer:
        update(_map.get(key))
          .flatMapT: (v, r) =>
            shortLock
              .lock(IO:
                updateMap(key, v))
              .rightAs(r)

  private def updateMap(key: K, value: V): Checked[Unit] =
    val checked = if _map.contains(key) then Checked.unit else onEntryInsert()
    for _ <- checked do
      _map = _map.updated(key, value)
    checked

  override def toString =
    s"$name(n=${_map.size})"


object AsyncMap:
  private val logger = Logger[this.type]

  def apply[K: Tag, V: Tag](initial: Map[K, V] = Map.empty[K, V]) =
    new AsyncMap(initial)

  def stoppable[K: Tag, V: Tag](initial: Map[K, V] = Map.empty[K, V]) =
    new AsyncMap(initial) with Stoppable

  def empty[K: Tag, V: Tag] =
    new AsyncMap(Map.empty[K, V])

  trait Stoppable:
    this: AsyncMap[?, ?] =>

    private val whenEmpty = Deferred.unsafe[IO, Unit]
    @volatile private var stoppingProblem: Problem | Null = null

    private def isStopping = stoppingProblem != null

    def isStoppingWith(problem: Problem): Boolean =
      problem == stoppingProblem

    final val whenStopped: IO[Unit] =
      memoize:
        whenEmpty.get
          .*>(IO(logger.debug(s"$name stopped")))

    final val stop: IO[Unit] =
      memoize:
        logger.traceIO(s"$name.stop"):
          initiateStop *> whenStopped

    /** Initiate stop. */
    final def initiateStop: IO[Unit] =
      initiateStopWithProblem(Problem.pure(s"$name is being stopped"))

    final def initiateStopWithProblemIfEmpty(problem: Problem): IO[Boolean] =
      IO.defer:
        logger.trace(s"$name initiateStopWithProblemIfEmpty $problem")
        shortLock
          .lock(IO:
            isEmpty && {
              stoppingProblem = problem
              true
            })
          .flatTap(IO.whenA(_):
            whenEmpty.complete(()).void)

    final def initiateStopWithProblem(problem: Problem): IO[Unit] =
      IO.defer:
        logger.trace(s"$name(initiateStopWithProblem $problem)")
        shortLock
          .lock(IO:
            stoppingProblem = problem
            isEmpty)
          .flatMap(IO.whenA(_):
            whenEmpty.complete(()).attempt.void)

    override protected[monixutils] final def onEntryInsert(): Checked[Unit] =
      stoppingProblem.toChecked

    override protected[monixutils] final def onEntryRemoved() =
      IO.defer(
        IO.whenA(isStopping && isEmpty)(
          whenEmpty.complete(()).attempt.void))
