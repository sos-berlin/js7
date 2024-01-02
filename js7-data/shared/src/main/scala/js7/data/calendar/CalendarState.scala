package js7.data.calendar

import js7.data.item.{SeparateTrivialItemState, UnsignedSimpleItemState}

/** Just for orthogonality. Calendar has no State. */
final case class CalendarState(item: Calendar)
extends UnsignedSimpleItemState, SeparateTrivialItemState[CalendarState]:

  protected type Self = CalendarState
  val companion: CalendarState.type = CalendarState


object CalendarState extends UnsignedSimpleItemState.Companion[CalendarState]:
  type Key = CalendarPath
  type Item = Calendar
  override type ItemState = CalendarState
