package js7.data.item

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec

case class TestPath(string: String) extends VersionedItemPath:
  def companion = TestPath


object TestPath extends VersionedItemPath.Companion[TestPath]:
  protected def unchecked(string: String) = new TestPath(string)


final case class TestVersionedItem(id: TestVersionedItem.Key, content: String)
extends VersionedItem, TrivialItemState[TestVersionedItem]:

  val companion: TestVersionedItem.type = TestVersionedItem

  def withId(id: VersionedItemId[Path]) = copy(id)


object TestVersionedItem
extends VersionedItem.Companion[TestVersionedItem], TrivialItemState.Companion[TestVersionedItem]:

  type Item = TestVersionedItem
  val cls = classOf[TestVersionedItem]

  type Path = TestPath
  val Path = TestPath

  override type Key = VersionedItemId[Path]

  implicit val jsonCodec: Codec.AsObject[TestVersionedItem] = deriveCodec[TestVersionedItem]
