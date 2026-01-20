package js7.data.event

import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import js7.data.event.EventDriven.*

/** An event driven object with a known key. */
trait EventDriven[+T <: EventDriven[T, E], E <: Event]:
  this: T =>

  type This <: T

  def companion: Companion[EventDriven[T, E], E]

  def applyEvent(event: E): Checked[T]

  final def applyEvents(events: IterableOnce[E]): Checked[T] =
    var obj = this
    val it = events.iterator
    while it.hasNext do
      val event = it.next()
      obj.applyEvent(event) match
        case Left(problem) =>
          return Left:
            problem |+| Problem(s"Event '$event' could not be applied to ${companion.name}")
        case Right(o) =>
          obj = o
    Right(obj)


object EventDriven:

  trait Companion[+O <: EventDriven[O, E], E <: Event]:

    val name: String = getClass.shortClassName

    given Companion[O, E] = this

    override def toString: String =
      name
