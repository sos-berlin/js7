package com.sos.scheduler.engine.minicom.remoting

import com.sos.scheduler.engine.common.scalautil.Collections.implicits.{RichTraversable, RichTraversableOnce}
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.minicom.idispatch.InvocableIDispatch.implicits._
import com.sos.scheduler.engine.minicom.idispatch.{DISPID, DispatchType, Invocable, InvocableFactory}
import com.sos.scheduler.engine.minicom.remoting.Remoting._
import com.sos.scheduler.engine.minicom.remoting.calls.{Call, CallCall, CreateInstanceCall, CreateInstanceResult, EmptyResult, GetIDsOfNamesCall, GetIDsOfNamesResult, InvokeCall, InvokeResult, ProxyId, ReleaseCall, Result}
import com.sos.scheduler.engine.minicom.remoting.proxy.{ClientRemoting, ProxyIDispatchFactory, ProxyRegister, SimpleProxyIDispatch}
import com.sos.scheduler.engine.minicom.remoting.serial.CallDeserializer._
import com.sos.scheduler.engine.minicom.remoting.serial.CallSerializer._
import com.sos.scheduler.engine.minicom.remoting.serial.ErrorSerializer._
import com.sos.scheduler.engine.minicom.remoting.serial.ResultSerializer._
import com.sos.scheduler.engine.minicom.remoting.serial.{ResultDeserializer, ServerRemoting}
import com.sos.scheduler.engine.minicom.types.{CLSID, IID}
import java.nio.ByteBuffer
import org.scalactic.Requirements._
import scala.collection.breakOut
import scala.util.control.NonFatal

/**
 * @author Joacim Zschimmer
 */
final class Remoting(
  connection: MessageConnection,
  invocableFactories: Iterable[InvocableFactory],
  proxyIDispatchFactories: Iterable[ProxyIDispatchFactory])
extends ServerRemoting with ClientRemoting {

  private val proxyRegister = new ProxyRegister
  private val createInvocable = toCreateInvocableByCLSID(invocableFactories)
  private val proxyClsidMap: Map[CLSID, ProxyIDispatchFactory.Fun] =
    (List(SimpleProxyIDispatch) ++ proxyIDispatchFactories).map { o ⇒ o.clsid → o.apply _ } (breakOut)

  def run() = while (processNextMessage()) {}

  private def processNextMessage(): Boolean =
    connection.receiveMessage() match {
      case Some(callBytes) ⇒
        val (resultBytes, n) = executeMessage(callBytes)
        connection.sendMessage(resultBytes, n)
        true
      case None ⇒
        false
    }

  private def executeMessage(callBuffer: ByteBuffer): (Array[Byte], Int) =
    try {
      val call = deserializeCall(this, callBuffer)
      val result = executeCall(call)
      serializeResult(proxyRegister, result)
    }
    catch { case NonFatal(t) ⇒
      logger.debug(t.toString, t)
      serializeError(t)
    }

  private def executeCall(call: Call): Result = call match {
    case CreateInstanceCall(clsid, outer, context, iids) ⇒
      require(outer == null && context == 0 && iids.size == 1)
      CreateInstanceResult(invocable = createInvocable(clsid, iids.head))

    case ReleaseCall(proxyId) ⇒
      proxyRegister.release(proxyId)
      EmptyResult

    case CallCall(proxyId, methodName, arguments) ⇒
      val invocable = proxyRegister.invocable(proxyId)
      val result = invocable.call(methodName, arguments)
      InvokeResult(result)
  }

  private[remoting] def newProxy(proxyId: ProxyId, name: String, proxyClsid: CLSID, properties: Iterable[(String, Any)]) = {
    val newProxy = proxyClsidMap.getOrElse(proxyClsid, proxyClsidMap(CLSID.Null))   // TODO getOrElse solange nicht alle Proxys implementiert sind
    val result = newProxy(this, proxyId, name, properties)
    proxyRegister.registerProxy(result)
    result
  }

  private[remoting] def invocable(proxyId: ProxyId) = proxyRegister.invocable(proxyId)

  private[remoting] def getIdOfName(proxyId: ProxyId, name: String) = {
    val call = GetIDsOfNamesCall(proxyId, IID.Null, localeId = 0, names = List(name))
    val GetIDsOfNamesResult(dispIds) = sendReceive(call).readGetIDsOfNamesResult()
    require(dispIds.size == 1)
    dispIds.head
  }

  private[remoting] def invoke(proxyId: ProxyId, dispId: DISPID, dispatchTypes: Set[DispatchType], arguments: Seq[Any], namedArguments: Seq[(DISPID, Any)]) = {
    val call = InvokeCall(proxyId, dispId, IID.Null, dispatchTypes, arguments.toImmutableSeq, namedArguments.toImmutableSeq)
    val InvokeResult(value) = sendReceive(call).readInvokeResult()
    value
  }

  private def sendReceive(call: Call): ResultDeserializer = {
    val (byteArray, length) = serializeCall(proxyRegister, call)
    connection.sendMessage(byteArray, length)
    val byteBuffer = connection.receiveMessage().get
    new ResultDeserializer(this, byteBuffer)
  }
}

object Remoting {
  private type CreateInvocableByCLSID = (CLSID, IID) ⇒ Invocable
  private val logger = Logger(getClass)

  private def toCreateInvocableByCLSID(invocableFactories: Iterable[InvocableFactory]): CreateInvocableByCLSID = {
    val clsidToFactoryMap = invocableFactories toKeyedMap { _.clsid }
    def createInvocable(clsId: CLSID, iid: IID): Invocable = {
      val factory = clsidToFactoryMap(clsId)
      require(factory.iid == iid, s"IID $iid is not supported by $factory")
      factory()
    }
    createInvocable  // Return the function itself
  }
}
