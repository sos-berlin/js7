package js7.base.catsutils

import cats.effect.{Resource, Sync}
import cats.effect.unsafe.IORuntime
import java.util.concurrent.ConcurrentHashMap
import js7.base.utils.CatsUtils.syntax.RichResource
import scala.concurrent.ExecutionContext

/** Only used for tests having their own IORuntimes. */
object OwnIORuntimeRegister:

  private val ecToRuntime = new ConcurrentHashMap[ExecutionContext, IORuntime]

  def toIORuntime(ec: ExecutionContext): Option[IORuntime] =
    Option(ecToRuntime.get(ec))

  def add(ec: ExecutionContext, ioRuntime: IORuntime): ExecutionContext =
    ecToRuntime.put(ec, ioRuntime)
    ec

  def remove(ec: ExecutionContext): Unit =
    ecToRuntime.remove(ec)

  def register[F[_]](ec: ExecutionContext, ioRuntime: IORuntime)(using F: Sync[F])
  : Resource[F, Unit] =
    Resource
      .make(
        acquire = F.delay:
          add(ec, ioRuntime))(
        release = ec => F.delay:
          remove(ec))
      .void
