package js7.proxy.javaapi.data.lock

import javax.annotation.Nonnull
import js7.data.lock.Acquired.Available
import js7.data.lock.{Lock, LockId, LockState}
import js7.data.order.OrderId
import js7.proxy.javaapi.data.common.JJsonable
import scala.jdk.CollectionConverters._

final case class JLockState(asScala: LockState)
extends JJsonable[JLockState]
{
  protected type AsScala = LockState
  protected def companion = JLockState

  @Nonnull
  def lockId: LockId =
    asScala.lock.id

  @Nonnull
  def lock: Lock =
    asScala.lock

  @Nonnull
  def isAvailable: Boolean =
    asScala.acquired == Available

  @Nonnull
  def orderIds: java.util.Collection[OrderId] =
    asScala.acquired.orderIds.asJavaCollection

  @Nonnull
  def queuedOrderIds: java.util.List[OrderId] =
    asScala.queue.asJava
}

object JLockState extends JJsonable.Companion[JLockState]
{
  protected def jsonEncoder = LockState.jsonCodec
  protected def jsonDecoder = LockState.jsonCodec
}
