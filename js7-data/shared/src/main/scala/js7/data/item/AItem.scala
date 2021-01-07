package js7.data.item

import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class AItem(id: VersionedItemId[APath], content: String) extends VersionedItem {
  type Self = AItem

  val companion = AItem

  def withId(id: VersionedItemId[APath]) = copy(id = id)
}

object AItem extends VersionedItem.Companion[AItem] {
  type ThisItem = AItem
  type Path = APath

  def itemPathCompanion = APath
}

case class APath(string: String) extends ItemPath
{
  def companion = APath
}

object APath extends ItemPath.Companion[APath]
{
  val sourceTypeToFilenameExtension = Map(
    SourceType.Json -> ".a.json",
    SourceType.Txt -> ".a.txt")

  protected[item] def unchecked(string: String) = new APath(string)
}
