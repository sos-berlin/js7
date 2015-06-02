package com.sos.scheduler.engine.minicom.remoting

import com.google.inject.Injector
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
import scala.annotation.tailrec
import scala.collection.{breakOut, immutable}
import scala.reflect.ClassTag
import scala.util.control.NonFatal

/**
 * @author Joacim Zschimmer
 */
final class Remoting(
  injector: Injector,
  dialogConnection: DialogConnection,
  invocableFactories: Iterable[InvocableFactory],
  proxyIDispatchFactories: Iterable[ProxyIDispatchFactory])
extends ServerRemoting with ClientRemoting {

  private val proxyRegister = new ProxyRegister
  private val createInvocable = toCreateInvocableByCLSID(invocableFactories)
  private val proxyClsidMap: Map[CLSID, ProxyIDispatchFactory.Fun] =
    (List(SimpleProxyIDispatch) ++ proxyIDispatchFactories).map { o ⇒ o.clsid → o.apply _ } (breakOut)

  def run(): Unit = continue(dialogConnection.receiveFirstMessage())

  @tailrec
  private def continue(messageOption: Option[ByteBuffer]): Unit =
    messageOption match {
      case Some(message) ⇒
        val (resultBytes, n) = executeMessage(message)
        val nextMessageOption = dialogConnection.sendAndReceive(resultBytes, n)
        continue(nextMessageOption)
      case None ⇒
    }

  private def executeMessage(callBuffer: ByteBuffer): (Array[Byte], Int) =
    try {
      val call = deserializeCall(this, callBuffer)
      val result = executeCall(call)
      serializeResult(proxyRegister, result)
    }
    catch {
      case NonFatal(t) ⇒
        logger.debug(t.toString, t)
        serializeError(t)
      case t: Throwable ⇒
        // Try to respond in any case (even LinkageError, OutOfMemoryError), to let the client know and continue
        logger.error(t.toString, t)
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
    logger.trace(s"getIdOfName $proxyId '${proxyRegister.invocable(proxyId)}' $name")
    val call = GetIDsOfNamesCall(proxyId, IID.Null, localeId = 0, names = List(name))
    val GetIDsOfNamesResult(dispIds) = sendReceive(call).readGetIDsOfNamesResult(1)
    dispIds.head
  }

  def invoke(proxyId: ProxyId, dispId: DISPID, dispatchTypes: Set[DispatchType], arguments: Seq[Any], namedArguments: Seq[(DISPID, Any)]) = {
    logger.trace(s"invoke $proxyId $dispId '${proxyRegister.invocable(proxyId)}' $dispatchTypes ${arguments.mkString("(", ",", ")")} ${(namedArguments map { case (k, v) ⇒ s"$k -> $v"}).mkString("(", ",", ")")}")
    val call = InvokeCall(proxyId, dispId, IID.Null, dispatchTypes, arguments.toImmutableSeq, namedArguments.toImmutableSeq)
    val InvokeResult(value) = sendReceive(call).readInvokeResult()
    value
  }

  private def sendReceive(call: Call): ResultDeserializer = {
    val (byteArray, length) = serializeCall(proxyRegister, call)
    val byteBuffer = dialogConnection.sendAndReceive(byteArray, length).get
    new ResultDeserializer(this, byteBuffer)
  }

  private def toCreateInvocableByCLSID(invocableFactories: Iterable[InvocableFactory]): CreateInvocableByCLSID = {
    val clsidToFactoryMap = invocableFactories toKeyedMap { _.clsid }
    def createInvocable(clsId: CLSID, iid: IID): Invocable = {
      val factory = clsidToFactoryMap(clsId)
      require(factory.iid == iid, s"IID $iid is not supported by $factory")
      injector.getInstance(factory.invocableClass)
    }
    createInvocable  // Return the function itself
  }

  /**
   * Returns all registered invocables implementing the erased type A.
   */
  def invocables[A : ClassTag]: immutable.Iterable[A] = proxyRegister.invocables[A]
}

object Remoting {
  private type CreateInvocableByCLSID = (CLSID, IID) ⇒ Invocable
  private val logger = Logger(getClass)
}
