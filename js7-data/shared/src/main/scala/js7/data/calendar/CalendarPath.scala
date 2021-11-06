package js7.data.calendar

import js7.base.annotation.javaApi
import js7.data.item.{InventoryItemPath, UnsignedSimpleItemPath}

final case class CalendarPath private(string: String)
extends UnsignedSimpleItemPath
with InventoryItemPath.AssignableToAgent
{
  protected type Self = CalendarPath

  val companion = CalendarPath
}

object CalendarPath extends UnsignedSimpleItemPath.Companion[CalendarPath]
{
  override val itemTypeName = Calendar.typeName

  protected def unchecked(string: String) = new CalendarPath(string)

  @javaApi
  def of(validName: String): CalendarPath =
    apply(validName)
}
