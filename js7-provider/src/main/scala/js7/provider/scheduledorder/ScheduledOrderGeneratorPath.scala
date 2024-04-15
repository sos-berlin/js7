package js7.provider.scheduledorder

import js7.data.item.{SourceType, VersionedItemPath}

/**
  * @author Joacim Zschimmer
  */
final case class ScheduledOrderGeneratorPath(string: String) extends VersionedItemPath:
  def companion: VersionedItemPath.Companion[_ <: VersionedItemPath] = ScheduledOrderGeneratorPath


object ScheduledOrderGeneratorPath extends VersionedItemPath.Companion[ScheduledOrderGeneratorPath]:
  override val sourceTypeToFilenameExtension: Map[SourceType, String] = Map(
    SourceType.Xml -> ".order.xml")

  protected def unchecked(string: String) = new ScheduledOrderGeneratorPath(string)
