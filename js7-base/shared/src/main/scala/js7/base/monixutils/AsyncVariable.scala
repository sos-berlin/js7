package js7.base.monixutils

import cats.effect.IO
import izumi.reflect.Tag
import js7.base.problem.Checked
import js7.base.utils.AsyncLock
import org.jetbrains.annotations.TestOnly

final class AsyncVariable[V] private(
  initial: V, varName: String, typeName: String, logMinor: Boolean):

  @volatile private var _value = initial
  private val lock = AsyncLock(varName, logMinor = logMinor)

  def get: V =
    _value

  def value: IO[V] =
    IO { _value }

  @TestOnly
  def lockedValue: IO[V] =
    lock.lock(value)

  def set(value: V)(implicit src: sourcecode.Enclosing): IO[V] =
    update(_ => IO.pure(value))

  def update(update: V => IO[V])(implicit src: sourcecode.Enclosing): IO[V] =
    shieldValue(
      for v <- update(_value) yield {
        _value = v
        v
      })

  def updateChecked(update: V => IO[Checked[V]])(implicit src: sourcecode.Enclosing)
  : IO[Checked[V]] =
    shieldValue:
      for checked <- update(_value) yield
        for v <- checked do _value = v
        checked

  def updateWithResult[R](update: V => IO[(V, R)])(implicit src: sourcecode.Enclosing): IO[R] =
    shieldValue:
      update(_value)
        .map: (v, r) =>
          _value = v
          r

  def updateCheckedWithResult[R](update: V => IO[Checked[(V, R)]])
    (implicit src: sourcecode.Enclosing)
  : IO[Checked[R]] =
    shieldValue:
      update(_value)
        .map(_.map: (v, r) =>
          _value = v
          r)

  //def use[R](body: V => IO[R])(implicit src: sourcecode.Enclosing): IO[R] =
  //  lock.lock(body)

  private def shieldValue[A](body: => IO[A])(implicit src: sourcecode.Enclosing): IO[A] =
    lock.lock(IO.defer/*shield access to _value in body*/(body))

  override lazy val toString = s"$varName: AsyncVariable[$typeName]"


object AsyncVariable:
  def apply[A](initial: A, logMinor: Boolean = false)
    (using src: sourcecode.Enclosing, tag: Tag[A])
  : AsyncVariable[A] =
    new AsyncVariable(initial, src.value, tag.tag.toString, logMinor = logMinor)
