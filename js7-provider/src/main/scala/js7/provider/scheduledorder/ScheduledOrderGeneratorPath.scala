package js7.provider.scheduledorder

import js7.data.item.{SourceType, VersionedItemPath}

/**
  * @author Joacim Zschimmer
  */
final case class ScheduledOrderGeneratorPath(string: String) extends VersionedItemPath
{
  def companion = ScheduledOrderGeneratorPath
}

object ScheduledOrderGeneratorPath extends VersionedItemPath.Companion[ScheduledOrderGeneratorPath]
{
  val sourceTypeToFilenameExtension = Map(
    SourceType.Xml -> ".order.xml")

  protected def unchecked(string: String) = new ScheduledOrderGeneratorPath(string)
}
