package js7.data.calendar

import js7.data.item.{SeparateTrivialItemState, UnsignedSimpleItemState}

/** Just for orthogonality. Calendar has no State. */
final case class CalendarState(item: Calendar)
extends UnsignedSimpleItemState
with SeparateTrivialItemState[CalendarState]
{
  protected type Self = CalendarState
  val companion = CalendarState
}

object CalendarState extends UnsignedSimpleItemState.Companion[CalendarState]
{
  type Path = CalendarPath
  type Item = Calendar
}
