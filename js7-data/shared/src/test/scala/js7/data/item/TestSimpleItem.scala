package js7.data.item

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec

case class TestSimpleId(string: String) extends UnsignedSimpleItemId {
  def companion = TestSimpleId
}

object TestSimpleId extends UnsignedSimpleItemId.Companion[TestSimpleId]
{
  protected def unchecked(string: String) = new TestSimpleId(string)

  val itemTypeName = "TestSimple"
}


final case class TestSimpleItem(
  id: TestSimpleId,
  content: String,
  itemRevision: Option[ItemRevision] = None)
extends UnsignedSimpleItem {
  type Self = TestSimpleItem
  val companion = TestSimpleItem

  def withId(id: TestSimpleId) = copy(id)

  def withRevision(revision: Option[ItemRevision]) =
    copy(itemRevision = revision)
}

object TestSimpleItem extends UnsignedSimpleItem.Companion[TestSimpleItem] {
  val cls = classOf[TestSimpleItem]

  type Path = TestSimpleId
  val Path = TestSimpleId

  override type Id = TestSimpleId
  val Id = TestSimpleId

  implicit val jsonCodec: Codec.AsObject[TestSimpleItem] = deriveCodec[TestSimpleItem]
}
