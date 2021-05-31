package js7.data.item

import io.circe.generic.semiauto.deriveCodec

/**
  * @author Joacim Zschimmer
  */
final case class AItem(id: VersionedItemId[APath], content: String) extends VersionedItem {
  type Self = AItem

  val companion = AItem

  def withId(id: VersionedItemId[APath]) = copy(id = id)

  def referencedItemPaths = Set.empty
}

object AItem extends VersionedItem.Companion[AItem] {
  type Item = AItem
  type Path = APath

  val cls = classOf[AItem]
  val Path = APath

  implicit val jsonCodec = deriveCodec[AItem]
}

case class APath(string: String) extends VersionedItemPath
{
  def companion = APath
}

object APath extends VersionedItemPath.Companion[APath]
{
  val sourceTypeToFilenameExtension = Map(
    SourceType.Json -> ".a.json",
    SourceType.Txt -> ".a.txt")

  protected[item] def unchecked(string: String) = new APath(string)
}
