package js7.data.event

import fs2.Stream
import js7.base.utils.MultipleLinesBracket.{Square, zipWithBracket}
import js7.base.utils.ScalaUtils.syntax.*

trait BasicState:

  type This <: BasicState_[This]

  def companion: BasicState.Companion[This]

  final def emitLineStream(emit: String => Unit): Unit =
    toLineStream.map(emit).compile.drain

  final def toLineStream: Stream[fs2.Pure, String] =
    toStringStream.through(fs2.text.lines)
      .zipWithBracket(Square)
      .map: (line, br) =>
        br +: line

  def toStringStream: Stream[fs2.Pure, String] =
    Stream.emit(toString)


trait BasicState_[T <: BasicState_[T]] extends BasicState:
  this: T =>
  override type This = T


object BasicState:

  trait Companion[T <: BasicState_[T]]:
    final given Companion[T] = this

    val name: String =
      getClass.shortClassName

    override def toString: String = name
