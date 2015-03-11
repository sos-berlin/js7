package com.sos.scheduler.engine.taskserver.spoolerapi

import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.minicom.idispatch.DISPID
import com.sos.scheduler.engine.minicom.remoting.calls.ProxyId
import com.sos.scheduler.engine.minicom.remoting.proxy.{ClientRemoting, ProxyIDispatchFactory, SpecializedProxyIDispatch}
import com.sos.scheduler.engine.minicom.types.CLSID
import java.util.UUID

/**
 * @author Joacim Zschimmer
 */
final class ProxySpoolerTask private(protected val remoting: ClientRemoting, val id: ProxyId, val name: String)
extends SpoolerTask with SpecializedProxyIDispatch {

  def setErrorCodeAndText(code: String, text: String): Unit = invokeMethod(DISPID(26), List(code, text))

  def paramsXml = invokeGet(DISPID(35)).asInstanceOf[String]

  def paramsXml_=(o: String) = invokePut(DISPID(35), o)

  def orderParamsXml = invokeGet(DISPID(36)).asInstanceOf[String]

  def orderParamsXml_=(o: String) = invokePut(DISPID(36), o)
}

object ProxySpoolerTask extends ProxyIDispatchFactory {
  val clsid = CLSID(UUID fromString "feee47aa-6c1b-11d8-8103-000476ee8afb")
  private val logger = Logger(getClass)

  def apply(remoting: ClientRemoting, id: ProxyId, name: String, properties: Iterable[(String, Any)]) = {
    if (properties.nonEmpty) logger.warn(s"IGNORED: $properties")  // TODO Weitere lokale Properties
    new ProxySpoolerTask(remoting, id, name)
  }
}
