package js7.provider.scheduledorder

import js7.data.item.{SourceType, ItemPath}

/**
  * @author Joacim Zschimmer
  */
final case class ScheduledOrderGeneratorPath(string: String) extends ItemPath
{
  def companion = ScheduledOrderGeneratorPath
}

object ScheduledOrderGeneratorPath extends ItemPath.Companion[ScheduledOrderGeneratorPath]
{
  val sourceTypeToFilenameExtension = Map(
    SourceType.Xml -> ".order.xml")

  protected def unchecked(string: String) = new ScheduledOrderGeneratorPath(string)
}
