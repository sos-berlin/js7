package com.sos.scheduler.engine.minicom.remoting

import akka.util.ByteString
import com.sos.scheduler.engine.base.generic.Completed
import com.sos.scheduler.engine.base.utils.ScalaUtils.cast
import com.sos.scheduler.engine.common.scalautil.Collections.implicits.{RichTraversable, RichTraversableOnce}
import com.sos.scheduler.engine.common.scalautil.Futures.implicits._
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.minicom.idispatch.IDispatch.implicits.RichIDispatch
import com.sos.scheduler.engine.minicom.idispatch.{DISPID, DispatchType, IDispatch, IUnknownFactory}
import com.sos.scheduler.engine.minicom.remoting.Remoting._
import com.sos.scheduler.engine.minicom.remoting.calls._
import com.sos.scheduler.engine.minicom.remoting.dialog.ClientDialogConnection
import com.sos.scheduler.engine.minicom.remoting.proxy.{ProxyIDispatchFactory, ProxyRegister, ProxyRemoting, SimpleProxyIDispatch}
import com.sos.scheduler.engine.minicom.remoting.serial.CallSerializer.serializeCall
import com.sos.scheduler.engine.minicom.remoting.serial.ErrorSerializer.serializeError
import com.sos.scheduler.engine.minicom.remoting.serial.ResultSerializer.serializeResult
import com.sos.scheduler.engine.minicom.remoting.serial.{CallDeserializer, ProxyRegistering, ProxyingIUnknownDeserializer, ResultDeserializer}
import com.sos.scheduler.engine.minicom.types.{CLSID, IID, IUnknown}
import org.scalactic.Requirements._
import scala.collection.{breakOut, immutable}
import scala.concurrent.{ExecutionContext, Future}
import scala.reflect.ClassTag
import scala.util.control.NonFatal

/**
 * @author Joacim Zschimmer
 */
trait Remoting extends ProxyRegistering with ProxyRemoting {

  protected val connection: ClientDialogConnection
  protected val name: String
  protected val iUnknownFactories: immutable.Iterable[IUnknownFactory]
  protected val proxyIDispatchFactories: immutable.Iterable[ProxyIDispatchFactory]
  protected implicit def executionContext: ExecutionContext

  protected def onReleased(iUnknown: IUnknown): Unit

  private val logger = Logger.withPrefix(getClass, name)
  private val proxyRegister = new ProxyRegister
  private lazy val createInvocable = toCreateIUnknownByCLSID(iUnknownFactories)
  private lazy val proxyClsidMap: Map[CLSID, ProxyIDispatchFactory.Fun] =
    (List(SimpleProxyIDispatch) ++ proxyIDispatchFactories).map { o ⇒ o.clsid → o.apply _ } (breakOut)

  protected final def executeMessage(callMessage: ByteString): ByteString =
    try {
      val call = deserializeCall(callMessage)
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
      CreateInstanceResult(iUnknown = createInvocable(clsid, iids.head))

    case ReleaseCall(proxyId) ⇒
      val released = proxyRegister.iUnknown(proxyId)
      proxyRegister.release(proxyId)
      onReleased(released)
      EmptyResult

    case _: QueryInterfaceCall | _: GetIDsOfNamesCall  ⇒
      throw new UnsupportedOperationException(call.getClass.getSimpleName)

    case InvokeCall(proxyId, dispatchId, iid, dispatchTypes, arguments, namedArguments) ⇒
      val iUnknown = cast[IDispatch](proxyRegister.iUnknown(proxyId))
      val result = iUnknown.invoke(dispatchId, dispatchTypes, arguments, namedArguments)
      InvokeResult(result)

    case CallCall(proxyId, methodName, arguments) ⇒
      val iUnknown = proxyRegister.iUnknown(proxyId)
      val result = iUnknown match {
        //case o: Invocable ⇒ o.call(methodName, arguments)
        case o: IDispatch ⇒ o.invokeMethod(methodName, arguments)
        case o ⇒ throw new IllegalArgumentException(s"Not an IDispatch or Invocable: ${o.getClass}")
      }
      InvokeResult(result)

    case KeepaliveCall ⇒ EmptyResult
  }

  private[remoting] final def newProxy(proxyId: ProxyId, name: String, proxyClsid: CLSID, properties: Iterable[(String, Any)]) = {
    val newProxy = proxyClsidMap.getOrElse(proxyClsid, throw new NoSuchElementException(s"No ProxyIDispatchFactory for CLSID $proxyClsid '$name'"))
    val result = newProxy(this, proxyId, name, properties)
    proxyRegister.registerProxy(result)
    result
  }

  private[remoting] final def iUnknown(proxyId: ProxyId): IUnknown =
    proxyRegister.iUnknown(proxyId)

  /**
   * Returns all registered iUnknowns implementing the erased type A.
   */
  final def iUnknowns[A <: IUnknown: ClassTag]: immutable.Iterable[A] =
    proxyRegister.iUnknowns[A]

  final def release(proxyId: ProxyId) = {
    logger.trace(s"release $proxyId '${proxyRegister.iUnknown(proxyId)}")
    for (response ← sendReceive(ReleaseCall(proxyId))) yield {
      response.readEmptyResult()
      Completed
    }
  }

  final def getIdOfName(proxyId: ProxyId, name: String) = {
    logger.trace(s"getIdOfName $proxyId '${proxyRegister.iUnknown(proxyId)}' $name")
    val call = GetIDsOfNamesCall(proxyId, IID.Null, localeId = 0, names = List(name))
    for (response ← sendReceive(call)) yield
      response.readGetIDsOfNamesResult(n = 1).dispatchIds.head
  }

  final def createInstance(clsid: CLSID, iid: IID): Future[IUnknown] = {
    for (response ← sendReceive(CreateInstanceCall(clsid, null, 0, List(iid)))) yield
      response.readCreateInstanceResult().iUnknown
  }

  final def invoke(proxyId: ProxyId, dispId: DISPID, dispatchTypes: Set[DispatchType], arguments: Seq[Any], namedArguments: Seq[(DISPID, Any)]) = {
    logger.trace(s"invoke $proxyId $dispId '${proxyRegister.iUnknown(proxyId)}' $dispatchTypes ${arguments.mkString("(", ",", ")")} ${(namedArguments map { case (k, v) ⇒ s"$k -> $v"}).mkString("(", ",", ")")}")
    val call = InvokeCall(proxyId, dispId, IID.Null, dispatchTypes, arguments.toImmutableSeq, namedArguments.toImmutableSeq)
    for (response ← sendReceive(call)) yield
      response.readInvokeResult().result
  }

  final def call(proxyId: ProxyId, methodName: String, arguments: Seq[Any]) = {
    val call = CallCall(proxyId, methodName, arguments.toImmutableSeq)
    for (response ← sendReceive(call)) yield
      response.readInvokeResult().result
  }

  final def sendReceiveKeepalive(): Future[Unit] =
    for (response ← sendReceive(KeepaliveCall))
      yield response.readEmptyResult()

  private def sendReceive(call: Call): Future[ResultDeserializer] = {
    def callbackLoop(sendByteString: ByteString): Future[ByteString] = {
      (for (byteString ← connection.sendAndReceive(sendByteString) map { _ getOrElse { throw new ConnectionClosedException }}) yield
        if (CallDeserializer.isCall(byteString)) {
          val callbackResult = executeMessage(byteString)
          callbackLoop(callbackResult)
        } else
          Future.successful(byteString)
      ).flatten
    }
    for (byteString ← callbackLoop(serializeCall(proxyRegister, call))) yield
      toResultDeserializer(byteString)
  }

  private def toResultDeserializer(byteString: ByteString): ResultDeserializer =
    new ResultDeserializer with ProxyingIUnknownDeserializer {
      protected val proxyRegistering = Remoting.this
      protected val buffer = byteString.toByteBuffer
    }

  protected final def deserializeCall(byteString: ByteString): Call =
    CallDeserializer.deserializeCall(this, byteString)
}

object Remoting {
  private type CreateIUnknownByCLSID = (CLSID, IID) ⇒ IUnknown
  final class ConnectionClosedException extends RuntimeException

  private def toCreateIUnknownByCLSID(invocableFactories: Iterable[IUnknownFactory]): CreateIUnknownByCLSID = {
    val clsidToFactoryMap = invocableFactories toKeyedMap { _.clsid }
    def createIUnknown(clsId: CLSID, iid: IID): IUnknown = {
      val factory = clsidToFactoryMap(clsId)
      require(factory.iid == iid, s"IID $iid is not supported by $factory")
      factory.newIUnknown()
    }
    createIUnknown  // Return the function itself
  }
}
