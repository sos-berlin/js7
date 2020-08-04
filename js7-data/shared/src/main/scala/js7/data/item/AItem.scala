package js7.data.item

import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class AItem(id: ItemId[APath], content: String) extends InventoryItem {
  type Self = AItem

  val companion = AItem

  def withId(id: ItemId[APath]) = copy(id = id)
}

object AItem extends InventoryItem.Companion[AItem] {
  type ThisItem = AItem
  type Path = APath

  def typedPathCompanion = APath
}

case class APath(string: String) extends TypedPath
{
  def companion = APath
}

object APath extends TypedPath.Companion[APath]
{
  val sourceTypeToFilenameExtension = Map(
    SourceType.Json -> ".a.json",
    SourceType.Txt -> ".a.txt")

  def unchecked(string: String) = new APath(string)
}
