package js7.data.item

import io.circe.generic.semiauto.deriveCodec

/**
  * @author Joacim Zschimmer
  */
final case class BItem(id: VersionedItemId[BPath], content: String) extends VersionedItem {
  type Self = BItem

  val companion = BItem

  def withId(id: VersionedItemId[BPath]) = copy(id = id)
}

object BItem extends VersionedItem.Companion[BItem] {
  type Item = BItem
  type Path = BPath

  val cls = classOf[BItem]
  val Path = BPath

  implicit val jsonCodec = deriveCodec[BItem]
}

case class BPath(string: String) extends ItemPath {
  def companion = BPath
}

object BPath extends ItemPath.Companion[BPath] {
  val sourceTypeToFilenameExtension: Map[SourceType, String] = Map(
    SourceType.Json -> ".b.json")

  protected def unchecked(string: String) = new BPath(string)
}
