package js7.base.catsutils

import cats.effect.unsafe.IORuntime
import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.ExecutionContext

/** Normally contains one the IORuntime, but tests may add their own IORuntimes. */
object OurIORuntimeRegister:
                            
  private val ecToRuntime = new ConcurrentHashMap[ExecutionContext, IORuntime]

  def toIORuntime(ec: ExecutionContext): Option[IORuntime] =
    Option(ecToRuntime.get(ec))

  def add(ec: ExecutionContext, ioRuntime: IORuntime): Unit =
    ecToRuntime.put(ec, ioRuntime)

  def remove(ec: ExecutionContext): Unit =
    ecToRuntime.remove(ec)
