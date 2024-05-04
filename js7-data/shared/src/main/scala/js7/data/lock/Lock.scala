package js7.data.lock

import io.circe.Codec
import js7.base.circeutils.CirceUtils.deriveConfiguredCodec
import js7.base.utils.Assertions.assertThat
import js7.data.item.{ItemRevision, UnsignedItemPath, UnsignedSimpleItem, UnsignedSimpleItemPath}

final case class Lock(
  path: LockPath,
  limit: Int = 1,
  itemRevision: Option[ItemRevision] = None)
extends UnsignedSimpleItem:
  protected type Self = Lock
  val companion: Lock.type = Lock

  assertThat(limit >= 0)

  def rename(path: LockPath): Lock =
    copy(path = path)

  def withRevision(revision: Option[ItemRevision]): Lock =
    copy(itemRevision = revision)

  def toInitialItemState: LockState =
    LockState(this)


object Lock extends UnsignedSimpleItem.Companion[Lock]:
  val cls: Class[Lock] = classOf[Lock]

  type Key = LockPath
  def Key: UnsignedSimpleItemPath.Companion[LockPath] = LockPath

  override type Path = LockPath
  override val Path: UnsignedItemPath.Companion[LockPath] = LockPath

  type ItemState = LockState

  given jsonCodec: Codec.AsObject[Lock] = deriveConfiguredCodec[Lock]
