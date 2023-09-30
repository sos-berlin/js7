package js7.base.monixutils

import cats.effect.Resource
import izumi.reflect.Tag
import js7.base.problem.Checked
import js7.base.utils.AsyncLock
import monix.eval.Task

final class AsyncVariable[V](initial: V, varName: String, typeName: String):

  @volatile private var _value = initial
  private val lock = AsyncLock(varName)

  def get: V =
    _value

  def value: Task[V] =
    Task { _value }

  def set(value: V)(implicit src: sourcecode.Enclosing): Task[V] =
    update(_ => Task.pure(value))

  def update(update: V => Task[V])(implicit src: sourcecode.Enclosing): Task[V] =
    shieldValue(
      for v <- update(_value) yield {
        _value = v
        v
      })

  def updateChecked(update: V => Task[Checked[V]])(implicit src: sourcecode.Enclosing)
  : Task[Checked[V]] =
    shieldValue(
      for checked <- update(_value) yield {
        for v <- checked do _value = v
        checked
      })

  def updateWithResult[R](update: V => Task[(V, R)])(implicit src: sourcecode.Enclosing): Task[R] =
    shieldValue(
      update(_value)
        .map { case (v, r) =>
          _value = v
          r
        })

  def updateCheckedWithResult[R](update: V => Task[Checked[(V, R)]])
    (implicit src: sourcecode.Enclosing)
  : Task[Checked[R]] =
    shieldValue(
      update(_value)
        .map(_.map { case (v, r) =>
          _value = v
          r
        }))

  def use[R](task: V => Task[R])(implicit src: sourcecode.Enclosing): Task[R] =
    resource.use(task)

  def resource(implicit src: sourcecode.Enclosing): Resource[Task, V] =
    lock.resource.map(_ => _value)

  private def shieldValue[A](body: => Task[A])(implicit src: sourcecode.Enclosing): Task[A] =
    lock.lock(Task.defer/*shield access to _value in body*/(body))

  override lazy val toString = s"$varName: AsyncVariable[$typeName]"

object AsyncVariable:
  def apply[A](initial: A)(implicit src: sourcecode.Enclosing, tag: Tag[A]) =
    new AsyncVariable(initial, src.value, tag.tag.toString)
