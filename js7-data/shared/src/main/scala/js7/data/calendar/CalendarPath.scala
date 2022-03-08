package js7.data.calendar

import js7.base.annotation.javaApi
import js7.data.item.{InventoryItemPath, UnsignedSimpleItemPath}

final case class CalendarPath private(string: String)
extends UnsignedSimpleItemPath
with InventoryItemPath.AttachableToAgent
{
  protected type Self = CalendarPath

  val companion = CalendarPath
}

object CalendarPath extends UnsignedSimpleItemPath.Companion[CalendarPath]
{
  // May deadlock: override val itemTypeName = Calendar.typeName

  protected def unchecked(string: String) = new CalendarPath(string)

  @javaApi
  def of(validName: String): CalendarPath =
    apply(validName)
}
