package js7.data.calendar

import js7.base.annotation.javaApi
import js7.data.agent.AgentPath.mayThrow
import js7.data.item.{InventoryItemPath, UnsignedSimpleItemPath}

final case class CalendarPath private(string: String)
extends UnsignedSimpleItemPath, InventoryItemPath.AttachableToAgent:

  protected type Self = CalendarPath

  val companion: CalendarPath.type = CalendarPath


object CalendarPath extends UnsignedSimpleItemPath.Companion[CalendarPath]:
  type Item = Calendar

  protected def unchecked(string: String) = new CalendarPath(string)

  @javaApi
  def of(validName: String): CalendarPath =
    mayThrow(validName)
