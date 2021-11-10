package js7.base.monixutils

import cats.effect.ExitCase
import cats.syntax.flatMap._
import js7.base.monixutils.AsyncMap.weirdProblem
import js7.base.problem.Problems.{DuplicateKey, UnknownKeyProblem}
import js7.base.problem.{Checked, Problem}
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

  // TODO Lesende Methoden sollen sofort bisherige Version liefern (keine Task)

  def all: Task[Map[K, V]] =
    mvarTask.flatMap(_.read)

  def removeAll: Task[Map[K, V]] =
    mvarTask.flatMap(mvar => mvar
      .take
      .flatMap(result => mvar.put(Map.empty).as(result)))
      .uncancelable

  def contains(key: K): Task[Boolean] =
    get(key).map(_.isDefined)

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

  def updateExisting(key: K, update: V => Task[Checked[V]]): Task[Checked[V]] =
    this.updateChecked(key, {
      case None => Task.pure(Left(UnknownKeyProblem(implicitClass[K].simpleScalaName, key)))
      case Some(existing) => update(existing)
    })

  // TODO On update, do not lock the whole Map, lock only the entry !

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

  def updateCheckedWithResult[R](key: K, update: Option[V] => Task[Checked[(V, R)]])
  : Task[Checked[R]] =
    Task.defer {
      @volatile var _result: Checked[R] = weirdProblem
      mvarTask
        .flatMap { mvar =>
          mvar.take.flatMap(map =>
            Task
              .defer/*catch inside task*/ {
                update(map.get(key))
                  .map(_.map { case (v, r) =>
                    _result = Right(r)
                    v
                  })
              }
              .guaranteeCase {
                case ExitCase.Completed => Task.unit
                case _ => mvar.put(map)
              }
              .flatTap(checked => mvar.put(checked match {
                case Left(_) => map
                case Right(value) => map + (key -> value)
              })))
        }
        .uncancelable
      .map(_.flatMap(_ => _result))
    }
}

object AsyncMap
{
  def apply[K: ClassTag, V](initial: Map[K, V] = Map.empty[K, V]) =
    new AsyncMap(initial)

  def empty[K: ClassTag, V] =
    new AsyncMap(Map.empty[K, V])

  private val weirdProblem = Left(Problem.pure("AsyncMap.updateCheckedWithResult without result"))
}
