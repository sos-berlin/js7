package com.sos.scheduler.engine.taskserver.spoolerapi

import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.data.message.MessageCode
import com.sos.scheduler.engine.minicom.idispatch.DISPID
import com.sos.scheduler.engine.minicom.idispatch.IDispatch.implicits._
import com.sos.scheduler.engine.minicom.remoting.calls.ProxyId
import com.sos.scheduler.engine.minicom.remoting.proxy.{ClientRemoting, ProxyIDispatchFactory, SpecializedProxyIDispatch}
import com.sos.scheduler.engine.minicom.types.CLSID
import java.util.UUID

/**
 * @author Joacim Zschimmer
 */
final class ProxySpoolerTask private(protected val remoting: ClientRemoting, val id: ProxyId, val name: String)
extends SpoolerTask with SpecializedProxyIDispatch {

  def setErrorCodeAndText(code: MessageCode, text: String): Unit =
    this.invokeMethod(DISPID(26), List(code.string, text))

  def paramsXml = this.invokeGet(DISPID(35)).asInstanceOf[String]

  def paramsXml_=(o: String) = this.invokePut(DISPID(35), o)

  def orderParamsXml = this.invokeGet(DISPID(36)).asInstanceOf[String]

  def orderParamsXml_=(o: String) = this.invokePut(DISPID(36), o)
}

object ProxySpoolerTask extends ProxyIDispatchFactory {
  val clsid = CLSID(UUID fromString "feee47aa-6c1b-11d8-8103-000476ee8afb")
  private val logger = Logger(getClass)

  def apply(remoting: ClientRemoting, id: ProxyId, name: String, properties: Iterable[(String, Any)]) = {
    if (properties.nonEmpty) logger.warn(s"IGNORED: $properties")  // TODO Weitere lokale Properties
    new ProxySpoolerTask(remoting, id, name)
  }
}
