package js7.data.item

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec

/**
  * @author Joacim Zschimmer
  */
final case class BItem(id: VersionedItemId[BPath], content: String)
extends VersionedItem, TrivialItemState[BItem]:

  val companion: BItem.type = BItem

  def withId(id: VersionedItemId[BPath]) = copy(id = id)


object BItem
extends VersionedItem.Companion[BItem], TrivialItemState.Companion[BItem]:

  type Item = BItem
  type Path = BPath

  val cls = classOf[BItem]
  val Path = BPath

  implicit val jsonCodec: Codec.AsObject[BItem] = deriveCodec[BItem]

case class BPath(string: String) extends VersionedItemPath:
  def companion = BPath


object BPath extends VersionedItemPath.Companion[BPath]:
  protected def unchecked(string: String) = new BPath(string)
