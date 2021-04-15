package js7.data.item

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec

case class TestSimpleId(string: String) extends SimpleItemId {
  def companion = TestSimpleId
}

object TestSimpleId extends SimpleItemId.Companion[TestSimpleId]
{
  protected def unchecked(string: String) = new TestSimpleId(string)

  val itemTypeName = "TestSimple"
}


final case class TestSimpleItem(
  id: TestSimpleId,
  content: String,
  itemRevision: Option[ItemRevision] = None)
extends SimpleItem {
  type Self = TestSimpleItem
  val companion = TestSimpleItem

  def withId(id: TestSimpleId) = copy(id)

  def withRevision(revision: ItemRevision) =
    copy(itemRevision = Some(revision))
}

object TestSimpleItem extends SimpleItem.Companion[TestSimpleItem] {
  val cls = classOf[TestSimpleItem]

  type Path = TestSimpleId
  val Path = TestSimpleId

  override type Id = TestSimpleId
  val Id = TestSimpleId

  implicit val jsonCodec: Codec.AsObject[TestSimpleItem] = deriveCodec[TestSimpleItem]
}
