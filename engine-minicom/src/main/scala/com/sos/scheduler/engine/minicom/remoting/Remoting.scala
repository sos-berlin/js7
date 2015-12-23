package com.sos.scheduler.engine.minicom.remoting

import akka.util.ByteString
import com.google.inject.Injector
import com.sos.scheduler.engine.common.scalautil.Collections.implicits.{RichTraversable, RichTraversableOnce}
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.minicom.idispatch.InvocableIDispatch.implicits._
import com.sos.scheduler.engine.minicom.idispatch.{DISPID, DispatchType, Invocable, InvocableFactory}
import com.sos.scheduler.engine.minicom.remoting.Remoting._
import com.sos.scheduler.engine.minicom.remoting.calls._
import com.sos.scheduler.engine.minicom.remoting.proxy.{ClientRemoting, ProxyIDispatchFactory, ProxyRegister, SimpleProxyIDispatch}
import com.sos.scheduler.engine.minicom.remoting.serial.CallDeserializer.deserializeCall
import com.sos.scheduler.engine.minicom.remoting.serial.CallSerializer.serializeCall
import com.sos.scheduler.engine.minicom.remoting.serial.ErrorSerializer.serializeError
import com.sos.scheduler.engine.minicom.remoting.serial.ResultSerializer.serializeResult
import com.sos.scheduler.engine.minicom.remoting.serial.{ResultDeserializer, ServerRemoting}
import com.sos.scheduler.engine.minicom.types.{CLSID, IID}
import java.time.{Duration, Instant}
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
  proxyIDispatchFactories: Iterable[ProxyIDispatchFactory],
  name: String,
  returnAfterReleaseOf: Invocable ⇒ Boolean = _ ⇒ false,
  keepaliveDurationOption: Option[Duration])
extends ServerRemoting with ClientRemoting {

  private val logger = Logger.withPrefix(getClass, name)
  private val proxyRegister = new ProxyRegister
  private val createInvocable = toCreateInvocableByCLSID(invocableFactories)
  private val proxyClsidMap: Map[CLSID, ProxyIDispatchFactory.Fun] =
    (List(SimpleProxyIDispatch) ++ proxyIDispatchFactories).map { o ⇒ o.clsid → o.apply _ } (breakOut)
  private var end = false

  def run(): Unit = {
    logger.debug("Started")
    val firstRequest = dialogConnection.receiveFirstMessage()
    keepaliveDurationOption match {
      case Some(keepaliveDuration) ⇒
        withKeepaliveThread(1.s max keepaliveDuration) {
          continue(firstRequest)
        }
      case None ⇒
        continue(firstRequest)
    }
    logger.debug("Ended")

    @tailrec
    def continue(messageOption: Option[ByteString]): Unit =
      messageOption match {
        case Some(message) ⇒
          val response = executeMessage(message)
          if (!end) {
            val nextMessageOption = dialogConnection.sendAndReceive(response)
            continue(nextMessageOption)
          } else
            dialogConnection.sendLastMessage(response)
        case None ⇒
      }
  }

  private def executeMessage(callMessage: ByteString): ByteString =
    try {
      val call = deserializeCall(this, callMessage)
      logger.debug(s"${call.getClass.getSimpleName}")
      logger.trace(s"$call")
      val result = executeCall(call)
      logger.debug(s"${result.getClass.getSimpleName}")
      logger.trace(s"$result")
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
      val released = proxyRegister.invocable(proxyId)
      proxyRegister.release(proxyId)
      end = returnAfterReleaseOf(released)
      EmptyResult

    case _: QueryInterfaceCall | _: GetIDsOfNamesCall | _: InvokeCall ⇒
      throw new UnsupportedOperationException(call.getClass.getSimpleName)

    case CallCall(proxyId, methodName, arguments) ⇒
      val invocable = proxyRegister.invocable(proxyId)
      val result = invocable.call(methodName, arguments)
      InvokeResult(result)

    case KeepaliveCall ⇒ EmptyResult
  }

  private[remoting] def newProxy(proxyId: ProxyId, name: String, proxyClsid: CLSID, properties: Iterable[(String, Any)]) = {
    val newProxy = proxyClsidMap.getOrElse(proxyClsid, proxyClsidMap(CLSID.Null))   // TODO getOrElse solange nicht alle Proxys implementiert sind
    val result = newProxy(injector, this, proxyId, name, properties)
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

  def sendReceiveKeepalive(): Unit = sendReceive(KeepaliveCall).readEmptyResult()

  private def sendReceive(call: Call): ResultDeserializer = {
    val callMessage = serializeCall(proxyRegister, call)
    val byteString = dialogConnection.sendAndReceive(callMessage) getOrElse { throw new ConnectionClosedException }
    new ResultDeserializer(this, byteString)
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

  private def withKeepaliveThread[A](duration: Duration)(body: ⇒ A): A = {
    val keepaliveThread = new KeepaliveThread(1.s max duration)
    keepaliveThread.start()
    try body
    finally {
      keepaliveThread.interrupt()
      keepaliveThread.join()
    }
  }

  private class KeepaliveThread(keepaliveDuration: Duration) extends Thread {
    setName("Remoting.Keepalive")

    override def run() =
      try {
        var t = Instant.now()
        while (true) {
          t += keepaliveDuration
          if (t > Instant.now()) sleepUntil(t)
          else t = Instant.now()
          sendReceiveKeepalive()
        }
      } catch {
        case _: InterruptedException ⇒
        case NonFatal(t) ⇒ logger.error(t.toString)
      }
  }
}

object Remoting {
  private type CreateInvocableByCLSID = (CLSID, IID) ⇒ Invocable
  final class ConnectionClosedException extends RuntimeException
}
