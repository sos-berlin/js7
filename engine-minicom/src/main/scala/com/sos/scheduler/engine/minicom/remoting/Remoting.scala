package com.sos.scheduler.engine.minicom.remoting

import akka.util.ByteString
import com.google.inject.Injector
import com.sos.scheduler.engine.common.scalautil.Collections.implicits.RichTraversableOnce
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.minicom.idispatch.{DISPID, DispatchType}
import com.sos.scheduler.engine.minicom.remoting.Remoting._
import com.sos.scheduler.engine.minicom.remoting.calls.{Call, CallCall, CreateInstanceCall, CreateInstanceResult, GetIDsOfNamesCall, InvokeCall, InvokeResult, KeepaliveCall, ProxyId, _}
import com.sos.scheduler.engine.minicom.remoting.dialog.ClientDialogConnection
import com.sos.scheduler.engine.minicom.remoting.proxy.{ClientRemoting, ProxyIDispatchFactory, ProxyRegister, SimpleProxyIDispatch}
import com.sos.scheduler.engine.minicom.remoting.serial.CallSerializer.serializeCall
import com.sos.scheduler.engine.minicom.remoting.serial.{CallDeserializer, ResultDeserializer, ServerRemoting}
import com.sos.scheduler.engine.minicom.types.{CLSID, IID, IUnknown}
import scala.annotation.tailrec
import scala.collection.{breakOut, immutable}
import scala.reflect.ClassTag

/**
 * @author Joacim Zschimmer
 */
trait Remoting extends ServerRemoting with ClientRemoting {

  protected val injector: Injector
  protected val connection: ClientDialogConnection
  protected val name: String
  protected val proxyIDispatchFactories: Iterable[ProxyIDispatchFactory]

  private val logger = Logger.withPrefix(getClass, name)
  protected final val proxyRegister = new ProxyRegister
  private val proxyClsidMap: Map[CLSID, ProxyIDispatchFactory.Fun] =
    (List(SimpleProxyIDispatch) ++ proxyIDispatchFactories).map { o ⇒ o.clsid → o.apply _ } (breakOut)

  private[remoting] final def newProxy(proxyId: ProxyId, name: String, proxyClsid: CLSID, properties: Iterable[(String, Any)]) = {
    val newProxy = proxyClsidMap.getOrElse(proxyClsid, proxyClsidMap(CLSID.Null))   // TODO getOrElse solange nicht alle Proxys implementiert sind
    val result = newProxy(injector, this, proxyId, name, properties)
    proxyRegister.registerProxy(result)
    result
  }

  private[remoting] final def iUnknown(proxyId: ProxyId) = proxyRegister.iUnknown(proxyId)

  private[remoting] final def getIdOfName(proxyId: ProxyId, name: String) = {
    logger.trace(s"getIdOfName $proxyId '${proxyRegister.iUnknown(proxyId)}' $name")
    val call = GetIDsOfNamesCall(proxyId, IID.Null, localeId = 0, names = List(name))
    val GetIDsOfNamesResult(dispIds) = sendReceive(call).readGetIDsOfNamesResult(1)
    dispIds.head
  }

  final def createInstance(clsid: CLSID, iid: IID): IUnknown = {
    val CreateInstanceResult(iUnknown) = sendReceive(CreateInstanceCall(clsid, null, 0, List(iid))).readCreateInstanceResult()
    iUnknown
  }

  final def invoke(proxyId: ProxyId, dispId: DISPID, dispatchTypes: Set[DispatchType], arguments: Seq[Any], namedArguments: Seq[(DISPID, Any)]) = {
    logger.trace(s"invoke $proxyId $dispId '${proxyRegister.iUnknown(proxyId)}' $dispatchTypes ${arguments.mkString("(", ",", ")")} ${(namedArguments map { case (k, v) ⇒ s"$k -> $v"}).mkString("(", ",", ")")}")
    val call = InvokeCall(proxyId, dispId, IID.Null, dispatchTypes, arguments.toImmutableSeq, namedArguments.toImmutableSeq)
    val InvokeResult(value) = sendReceive(call).readInvokeResult()
    value
  }

  final def call(proxyId: ProxyId, methodName: String, arguments: Seq[Any]): Any = {
    val call = CallCall(proxyId, methodName, arguments.toImmutableSeq)
    val InvokeResult(value) = sendReceive(call).readInvokeResult()
    value
  }

  final def sendReceiveKeepalive(): Unit = sendReceive(KeepaliveCall).readEmptyResult()

  private def sendReceive(call: Call): ResultDeserializer = {
    @tailrec def callbackLoop(sendByteString: ByteString): ByteString = {
      val byteString = connection.sendAndReceive(sendByteString) getOrElse { throw new ConnectionClosedException }
      if (CallDeserializer.isCall(byteString)) {
        val callbackResult = executeMessage(byteString)
        callbackLoop(callbackResult)
      } else
        byteString
    }
    val byteString = callbackLoop(serializeCall(proxyRegister, call))
    new ResultDeserializer(this, byteString)
  }

  /**
   * Returns all registered iUnknowns implementing the erased type A.
   */
  final def iUnknowns[A <: IUnknown: ClassTag]: immutable.Iterable[A] = proxyRegister.iUnknowns[A]
}

object Remoting {
  final class ConnectionClosedException extends RuntimeException
}
