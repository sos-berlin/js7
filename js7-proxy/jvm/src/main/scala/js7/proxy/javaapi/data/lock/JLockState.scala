package js7.proxy.javaapi.data.lock

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

  def lockId: LockId =
    asScala.lock.id

  def lock: Lock =
    asScala.lock

  def isAvailable: Boolean =
    asScala.acquired == Available

  def orderIds: java.util.Collection[OrderId] =
    asScala.acquired.orderIds.asJavaCollection

  def queuedOrderIds: java.util.List[OrderId] =
    asScala.queue.asJava
}

object JLockState extends JJsonable.Companion[JLockState]
{
  protected def jsonEncoder = LockState.jsonCodec
  protected def jsonDecoder = LockState.jsonCodec
}
