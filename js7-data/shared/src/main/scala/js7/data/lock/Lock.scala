package js7.data.lock

import io.circe.Codec
import js7.base.circeutils.CirceUtils.deriveConfiguredCodec
import js7.base.utils.Assertions.assertThat
import js7.data.item.{ItemRevision, UnsignedSimpleItem}

final case class Lock(
  path: LockPath,
  limit: Int = 1,
  itemRevision: Option[ItemRevision] = None)
extends UnsignedSimpleItem:
  protected type Self = Lock
  val companion: Lock.type = Lock

  assertThat(limit >= 0)

  def rename(path: LockPath) =
    copy(path = path)

  def withRevision(revision: Option[ItemRevision]) =
    copy(itemRevision = revision)

  def toInitialItemState: LockState =
    LockState(this)


object Lock extends UnsignedSimpleItem.Companion[Lock]:
  val cls = classOf[Lock]

  type Key = LockPath
  def Key = LockPath

  override type Path = LockPath
  override val Path = LockPath

  type ItemState = LockState

  val jsonCodec: Codec.AsObject[Lock] = deriveConfiguredCodec[Lock]
