package js7.data.lock

import io.circe.generic.extras.Configuration.default.withDefaults
import js7.base.circeutils.CirceUtils.deriveConfiguredCodec
import js7.base.utils.Assertions.assertThat
import js7.data.item.{ItemRevision, SimpleItem}

final case class Lock(
  id: LockId,
  limit: Int = 1,
  itemRevision: Option[ItemRevision] = None)
extends SimpleItem
{
  protected type Self = Lock
  val companion = Lock

  assertThat(limit >= 0)

  def withRevision(revision: ItemRevision) =
    copy(itemRevision = Some(revision))
}

object Lock extends SimpleItem.Companion
{
  type Item = Lock
  type Id = LockId

  val cls = classOf[Lock]
  val idCompanion = LockId

  val jsonCodec = {
    implicit val configuration = withDefaults
    deriveConfiguredCodec[Lock]
  }
}
