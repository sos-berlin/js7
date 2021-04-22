package  js7.data_for_java.lock

import io.vavr.control.{Either => VEither}
import javax.annotation.Nonnull
import js7.base.problem.Problem
import js7.data.lock.{Lock, LockPath}
import js7.data_for_java.common.JJsonable
import js7.data_for_java.item.JUnsignedSimpleItem

final case class JLock(asScala: Lock)
extends JJsonable[JLock] with JUnsignedSimpleItem
{
  protected type AsScala = Lock
  protected def companion = JLock

  @Nonnull
  def path: LockPath =
    asScala.id

  def limit: Int =
    asScala.limit
}

object JLock extends JJsonable.Companion[JLock]
{
  @Nonnull
  def of(@Nonnull lockPath: LockPath, limit: Int): JLock =
    JLock(Lock(lockPath, limit))

  @Nonnull
  override def fromJson(jsonString: String): VEither[Problem, JLock] =
    super.fromJson(jsonString)

  protected def jsonEncoder = Lock.jsonCodec
  protected def jsonDecoder = Lock.jsonCodec
}
