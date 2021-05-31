package js7.data.item

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec

case class TestSimplePath(string: String) extends UnsignedSimpleItemPath {
  def companion = TestSimplePath
}

object TestSimplePath extends UnsignedSimpleItemPath.Companion[TestSimplePath]
{
  protected def unchecked(string: String) = new TestSimplePath(string)

  val itemTypeName = "TestSimple"
}


final case class TestSimpleItem(
  path: TestSimplePath,
  content: String,
  itemRevision: Option[ItemRevision] = None)
extends UnsignedSimpleItem {
  type Self = TestSimpleItem
  val companion = TestSimpleItem

  def withId(id: TestSimplePath) = copy(id)

  def withRevision(revision: Option[ItemRevision]) =
    copy(itemRevision = revision)

  def referencedItemPaths = Set.empty
}

object TestSimpleItem extends UnsignedSimpleItem.Companion[TestSimpleItem] {
  val cls = classOf[TestSimpleItem]

  type Path = TestSimplePath
  val Path = TestSimplePath

  override type Key = TestSimplePath
  val Key = TestSimplePath

  implicit val jsonCodec: Codec.AsObject[TestSimpleItem] = deriveCodec[TestSimpleItem]
}
