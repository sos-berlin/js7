package com.sos.scheduler.engine.taskserver.spoolerapi

import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.data.log.SchedulerLogLevel
import com.sos.scheduler.engine.minicom.idispatch.DISPID
import com.sos.scheduler.engine.minicom.remoting.calls.ProxyId
import com.sos.scheduler.engine.minicom.remoting.proxy.{ClientRemoting, ProxyIDispatchFactory, SpecializedProxyIDispatch}
import com.sos.scheduler.engine.minicom.types.CLSID
import java.util.UUID

/**
 * @author Joacim Zschimmer
 */
final class ProxySpoolerLog private(protected val remoting: ClientRemoting, val id: ProxyId, val name: String)
extends SpoolerLog with SpecializedProxyIDispatch {

  def log(level: SchedulerLogLevel, message: String) = invokeMethod(DISPID(14), Vector(level.cppNumber, message))
}

object ProxySpoolerLog extends ProxyIDispatchFactory {
  val clsid = CLSID(UUID fromString "feee47a6-6c1b-11d8-8103-000476ee8afb")
  private val logger = Logger(getClass)

  def apply(remoting: ClientRemoting, id: ProxyId, name: String, properties: Iterable[(String, Any)]) = {
    if (properties.nonEmpty) logger.warn(s"IGNORED: $properties")  // ???
    new ProxySpoolerLog(remoting, id, name)
  }
}
