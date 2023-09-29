package js7.data.item

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec

/**
  * @author Joacim Zschimmer
  */
final case class AItem(id: VersionedItemId[APath], content: String)
extends VersionedItem
with TrivialItemState[AItem]:
  protected type Self = AItem
  val item: AItem = this

  val companion: AItem.type = AItem

  def withId(id: VersionedItemId[APath]) = copy(id = id)

object AItem
extends VersionedItem.Companion[AItem]
with TrivialItemState.Companion[AItem]:
  type Item = AItem
  type Path = APath

  val cls = classOf[AItem]
  val Path = APath

  implicit val jsonCodec: Codec.AsObject[AItem] = deriveCodec[AItem]

case class APath(string: String) extends VersionedItemPath:
  def companion = APath

object APath extends VersionedItemPath.Companion[APath]:
  override val sourceTypeToFilenameExtension = Map(
    SourceType.Json -> ".a.json",
    SourceType.Txt -> ".a.txt")

  protected[item] def unchecked(string: String) = new APath(string)
