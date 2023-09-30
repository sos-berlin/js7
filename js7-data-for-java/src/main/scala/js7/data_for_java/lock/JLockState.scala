package js7.data_for_java.lock

import javax.annotation.Nonnull
import js7.data.lock.Acquired.Available
import js7.data.lock.{Lock, LockPath, LockState}
import js7.data.order.OrderId
import js7.data_for_java.common.JJsonable
import scala.jdk.CollectionConverters.*

final case class JLockState(asScala: LockState)
extends JJsonable[JLockState]:

  type AsScala = LockState
  protected def companion = JLockState

  @Nonnull
  def lockPath: LockPath =
    asScala.lock.path

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

object JLockState extends JJsonable.Companion[JLockState]:
  type AsScala = LockState

  protected def jsonEncoder = LockState.jsonCodec
  protected def jsonDecoder = LockState.jsonCodec
