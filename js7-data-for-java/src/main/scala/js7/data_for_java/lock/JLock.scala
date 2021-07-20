package js7.data_for_java.lock

import io.vavr.control.{Either => VEither}
import java.util.Optional
import javax.annotation.Nonnull
import js7.base.problem.Problem
import js7.data.item.ItemRevision
import js7.data.lock.{Lock, LockPath}
import js7.data_for_java.common.JJsonable
import js7.data_for_java.item.JUnsignedSimpleItem
import scala.jdk.OptionConverters._

final case class JLock(asScala: Lock)
extends JJsonable[JLock] with JUnsignedSimpleItem
{
  protected type AsScala = Lock
  protected def companion = JLock

  @Nonnull
  def path: LockPath =
    asScala.path

  def limit: Int =
    asScala.limit

  @Nonnull
  def withRevision(revision: Optional[ItemRevision]) =
    copy(asScala.withRevision(revision.toScala))
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
