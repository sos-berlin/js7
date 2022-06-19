package js7.data.calendar

import js7.data.item.{TrivialItemState, UnsignedSimpleItemState}

/** Just for orthogonality. Calendar has no State. */
final case class CalendarState(item: Calendar)
extends UnsignedSimpleItemState with TrivialItemState {
  protected type Self = CalendarState
  val companion = CalendarState
}

object CalendarState extends UnsignedSimpleItemState.Companion[CalendarState]
{
  type Path = CalendarPath
  type ItemState = CalendarState
  type Item = Calendar
}
