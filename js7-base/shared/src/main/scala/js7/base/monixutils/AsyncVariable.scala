package js7.base.monixutils

import cats.effect.IO
import izumi.reflect.Tag
import js7.base.problem.Checked
import js7.base.utils.AsyncLock
import org.jetbrains.annotations.TestOnly

final class AsyncVariable[V] private(
  initial: V, varName: String, typeName: String, suppressLog: Boolean = false, logMinor: Boolean):

  @volatile private var _value = initial
  private val lock = AsyncLock(varName, suppressLog = suppressLog, logMinor = logMinor)

  def get: V =
    _value

  def value: IO[V] =
    IO(_value)

  @TestOnly
  def lockedValue: IO[V] =
    lock.lock(value)

  /** Lock and use the value without changing it. */
  def use[A](body: V => IO[A]): IO[A] =
    shieldValue:
      IO.defer(body(_value))

  def set(value: V)(using sourcecode.Enclosing): IO[V] =
    update(_ => IO.pure(value))

  def updateDirect(update: V => V)(using sourcecode.Enclosing): IO[V] =
    this.update(v => IO.pure(update(v)))

  def update(update: V => IO[V])(using sourcecode.Enclosing): IO[V] =
    shieldValue:
      for v <- update(_value) yield
        _value = v
        v

  def updateChecked(update: V => IO[Checked[V]])(using sourcecode.Enclosing): IO[Checked[V]] =
    shieldValue:
      for checked <- update(_value) yield
        for v <- checked do _value = v
        checked

  def updateWithResult[R](update: V => IO[(V, R)])(using sourcecode.Enclosing): IO[R] =
    shieldValue:
      update(_value)
        .map: (v, r) =>
          _value = v
          r

  def updateCheckedWithResult[R](update: V => IO[Checked[(V, R)]])(using sourcecode.Enclosing)
  : IO[Checked[R]] =
    shieldValue:
      update(_value)
        .map(_.map: (v, r) =>
          _value = v
          r)

  //def use[R](body: V => IO[R])(using sourcecode.Enclosing): IO[R] =
  //  lock.lock(body)

  private def shieldValue[A](body: => IO[A])(using sourcecode.Enclosing): IO[A] =
    lock.lock:
      IO.defer: /*shield access to _value in body*/
        body

  override lazy val toString = s"$varName: AsyncVariable[$typeName]"


object AsyncVariable:
  def apply[A](
    initial: A,
    suppressLog: Boolean = false,
    logMinor: Boolean = false)
    (using src: sourcecode.Enclosing, tag: Tag[A])
  : AsyncVariable[A] =
    new AsyncVariable(initial, src.value, tag.tag.toString,
      suppressLog = suppressLog, logMinor = logMinor)
