package com.sos.jobscheduler.provider.scheduledorder

import com.sos.jobscheduler.data.filebased.{SourceType, TypedPath}

/**
  * @author Joacim Zschimmer
  */
final case class ScheduledOrderGeneratorPath(string: String) extends TypedPath
{
  def companion = ScheduledOrderGeneratorPath
}

object ScheduledOrderGeneratorPath extends TypedPath.Companion[ScheduledOrderGeneratorPath]
{
  val sourceTypeToFilenameExtension = Map(
    SourceType.Xml -> ".order.xml")

  protected def unchecked(string: String) = new ScheduledOrderGeneratorPath(string)
}
