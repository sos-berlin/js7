package js7.proxy.javaapi.data.lock

import io.vavr.control.{Either => VEither}
import javax.annotation.Nonnull
import js7.base.problem.Problem
import js7.data.lock.{Lock, LockId}
import js7.proxy.javaapi.data.common.JJsonable
import js7.proxy.javaapi.data.item.JSimpleItem

final case class JLock(asScala: Lock)
extends JJsonable[JLock] with JSimpleItem
{
  protected type AsScala = Lock
  protected def companion = JLock

  @Nonnull
  def id: LockId =
    asScala.id

  def limit: Int =
    asScala.limit
}

object JLock extends JJsonable.Companion[JLock]
{
  @Nonnull
  override def fromJson(jsonString: String): VEither[Problem, JLock] =
    super.fromJson(jsonString)

  protected def jsonEncoder = Lock.jsonCodec
  protected def jsonDecoder = Lock.jsonCodec
}
