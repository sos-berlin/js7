package js7.data.lock

import io.circe.generic.extras.Configuration.default.withDefaults
import js7.base.circeutils.CirceUtils.deriveConfiguredCodec
import js7.base.utils.Assertions.assertThat
import js7.data.item.{ItemRevision, UnsignedSimpleItem}

final case class Lock(
  path: LockPath,
  limit: Int = 1,
  itemRevision: Option[ItemRevision] = None)
extends UnsignedSimpleItem
{
  protected type Self = Lock
  val companion = Lock

  assertThat(limit >= 0)

  def rename(path: LockPath) =
    copy(path = path)

  def withRevision(revision: Option[ItemRevision]) =
    copy(itemRevision = revision)

  def toInitialItemState = LockState(this)
}

object Lock extends UnsignedSimpleItem.Companion[Lock]
{
  val cls = classOf[Lock]

  type Key = LockPath
  val Key = LockPath

  override type Path = LockPath
  override val Path = LockPath

  type ItemState = LockState

  val jsonCodec = {
    implicit val configuration = withDefaults
    deriveConfiguredCodec[Lock]
  }
}
