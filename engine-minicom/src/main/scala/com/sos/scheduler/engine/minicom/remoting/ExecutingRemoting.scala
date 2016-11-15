package com.sos.scheduler.engine.minicom.remoting

import akka.util.ByteString
import com.google.inject.Injector
import com.sos.scheduler.engine.base.utils.ScalaUtils.cast
import com.sos.scheduler.engine.common.scalautil.Collections.implicits.RichTraversable
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.minicom.idispatch.IDispatch.implicits.RichIDispatch
import com.sos.scheduler.engine.minicom.idispatch.{IDispatch, IUnknownFactory}
import com.sos.scheduler.engine.minicom.remoting.ExecutingRemoting._
import com.sos.scheduler.engine.minicom.remoting.calls.{Call, CallCall, CreateInstanceCall, CreateInstanceResult, EmptyResult, GetIDsOfNamesCall, InvokeCall, InvokeResult, KeepaliveCall, QueryInterfaceCall, ReleaseCall, Result}
import com.sos.scheduler.engine.minicom.remoting.proxy.ProxyRegister
import com.sos.scheduler.engine.minicom.remoting.serial.CallDeserializer.deserializeCall
import com.sos.scheduler.engine.minicom.remoting.serial.ErrorSerializer.serializeError
import com.sos.scheduler.engine.minicom.remoting.serial.ResultSerializer.serializeResult
import com.sos.scheduler.engine.minicom.types.{CLSID, IID, IUnknown}
import org.scalactic.Requirements._
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
private[remoting] trait ExecutingRemoting {
  this: Remoting ⇒

  protected val injector: Injector
  protected val proxyRegister: ProxyRegister
  protected def onReleased(iUnknown: IUnknown): Unit

  private lazy val createInvocable = toCreateIUnknownByCLSID(invocableFactories)
  protected val invocableFactories: Iterable[IUnknownFactory]

  override private[remoting] def executeMessage(callMessage: ByteString): ByteString =
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

  private def toCreateIUnknownByCLSID(invocableFactories: Iterable[IUnknownFactory]): CreateIUnknownByCLSID = {
    val clsidToFactoryMap = invocableFactories toKeyedMap { _.clsid }
    def createIUnknown(clsId: CLSID, iid: IID): IUnknown = {
      val factory = clsidToFactoryMap(clsId)
      require(factory.iid == iid, s"IID $iid is not supported by $factory")
      injector.getInstance(factory.iUnknownClass)
    }
    createIUnknown  // Return the function itself
  }
}

object ExecutingRemoting {
  private type CreateIUnknownByCLSID = (CLSID, IID) ⇒ IUnknown
  private val logger = Logger(getClass)
}
