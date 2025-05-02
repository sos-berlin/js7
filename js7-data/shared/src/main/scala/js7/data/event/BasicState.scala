package js7.data.event

import js7.base.utils.ScalaUtils.syntax.*
import js7.data.event.BasicState.*
import fs2.Stream
import js7.base.utils.MultipleLinesBracket.{Square, zipWithBracket}

trait BasicState[S <: BasicState[S]]:
  def companion: Companion[S]

  final def emitLineStream(emit: String => Unit): Unit =
    toLineStream.map(emit).compile.drain

  final def toLineStream: Stream[fs2.Pure, String] =
    toStringStream.through(fs2.text.lines)
      .zipWithBracket(Square)
      .map: (line, br) =>
        br +: line

  def toStringStream: Stream[fs2.Pure, String] =
    Stream.emit(toString)


object BasicState:
  trait Companion[S <: BasicState[S]]:
    implicit final val implicitBasicState: Companion[S] =
      this

    val name: String =
      getClass.shortClassName

    override def toString: String = name
