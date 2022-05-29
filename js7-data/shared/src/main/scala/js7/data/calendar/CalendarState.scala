package js7.data.calendar

import js7.data.item.UnsignedSimpleItemState
import monix.reactive.Observable

/** Just for orthogonality. Calendar has no State. */
final case class CalendarState(item: Calendar) extends UnsignedSimpleItemState {
  protected type Item = Calendar
  protected type Self = CalendarState
  val companion = CalendarState

  override def toSnapshotObservable = Observable.pure(item)
}

object CalendarState extends UnsignedSimpleItemState.Companion[CalendarState]
{
  type Path = CalendarPath
}
