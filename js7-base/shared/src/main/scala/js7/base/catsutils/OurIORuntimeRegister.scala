package js7.base.catsutils

import cats.effect.unsafe.IORuntime
import cats.effect.{Resource, Sync}
import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.ExecutionContext

/** Only used for tests having their own IORuntimes. */
object OurIORuntimeRegister:

  private val ecToRuntime = new ConcurrentHashMap[ExecutionContext, IORuntime]

  def toIORuntime(ec: ExecutionContext): Option[IORuntime] =
    Option(ecToRuntime.get(ec))

  def add(ec: ExecutionContext, ioRuntime: IORuntime): Unit =
    ecToRuntime.put(ec, ioRuntime)

  def remove(ec: ExecutionContext): Unit =
    ecToRuntime.remove(ec)

  def register[F[_]](ec: ExecutionContext, ioRuntime: IORuntime)(using F: Sync[F])
  : Resource[F, Unit] =
    Resource:
      F.delay:
        add(ec, ioRuntime) -> F.delay:
          remove(ec)
