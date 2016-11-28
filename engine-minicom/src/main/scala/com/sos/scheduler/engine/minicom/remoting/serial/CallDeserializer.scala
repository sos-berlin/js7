package com.sos.scheduler.engine.minicom.remoting.serial

import akka.util.ByteString
import com.sos.scheduler.engine.minicom.idispatch.{DISPID, DispatchType}
import com.sos.scheduler.engine.minicom.remoting.calls._
import com.sos.scheduler.engine.minicom.remoting.serial.CallDeserializer._
import com.sos.scheduler.engine.minicom.types.{CLSID, IID}
import scala.collection.immutable

/**
 * @author Joacim Zschimmer
 */
private[serial] trait CallDeserializer extends VariantDeserializer {

  final def readCallAndEnd(): Call = {
    val result = readCall()
    requireEndOfMessage()
    result
  }

  private final def readCall(): Call =
    readByte() match {
      case MessageClass.Session ⇒ readSessionCall()
      case MessageClass.Object ⇒ readObjectCall()
      case MessageClass.KeepAlive ⇒ KeepaliveCall
    }

  private def readSessionCall(): Call = {
    require(readInt64() == 0, "Session ID is not 0")
    readByte() match {
      case MessageCommand.CreateInstance ⇒
        val clsid = CLSID(readUUID())
        val outer = readIUnknownOrNull()
        val context = readInt32()
        val n = readInt32()
        val iids = immutable.Seq.fill(n) { IID(readUUID()) }
        CreateInstanceCall(clsid, outer, context, iids)
    }
  }

  private def readObjectCall(): ObjectCall = {
    val proxyId = ProxyId(readInt64())
    readByte() match {

      case MessageCommand.Release ⇒
        ReleaseCall(proxyId)

      case MessageCommand.QueryInterface ⇒
        val iid = IID(readUUID())
        QueryInterfaceCall(proxyId, iid)

      case MessageCommand.GetIDsOfNames ⇒
        val iid = IID(readUUID())
        val localeId = readInt32()
        val names = immutable.Seq.fill(readInt32()) { readString() }
        GetIDsOfNamesCall(proxyId, iid, localeId, names)

      case MessageCommand.Invoke ⇒
        val dispatchId = DISPID(readInt32())
        val iid = IID(readUUID())
        val localeId = readInt32()
        require(localeId == 0)
        val flags = readInt32()
        val n = readInt32()
        val namedArgumentCount = readInt32()
        val argDispatchIds = Vector.fill(namedArgumentCount) { DISPID(readInt32()) }
        val namedArguments = (argDispatchIds map { _ → readVariant() }).reverse
        val arguments = readArguments(n - namedArgumentCount)
        InvokeCall(proxyId, dispatchId, iid, DispatchType.set(flags), arguments, namedArguments)

      case MessageCommand.Call ⇒
        val methodName = readString()
        val argumentCount = readInt32()
        val namedArgumentCount = readInt32()
        require(namedArgumentCount == 0)
        val arguments = readArguments(argumentCount)
        CallCall(proxyId, methodName, arguments)
    }
  }

  private def readArguments(n: Int): immutable.Seq[Any] = Vector.fill(n) { readVariant() } .reverse
}

object CallDeserializer {
  private[remoting] object MessageCommand {
    val CreateInstance: Byte = 'C'
    val Release       : Byte = 'R'
    val QueryInterface: Byte = 'Q'
    val GetIDsOfNames : Byte = 'G'
    val Invoke        : Byte = 'I'
    val Call          : Byte = 'A'
  }

  private object NoProxyingRemoting extends Proxying {
    private[remoting] def iUnknown(proxyId: ProxyId) =
      throw new UnsupportedOperationException("No Proxying")

    private[remoting] def newProxy(proxyId: ProxyId, name: String, proxyClsid: CLSID, properties: Iterable[(String, Any)]) =
      throw new UnsupportedOperationException("No Proxying")
  }

  def messageIsCall(message: ByteString): Boolean =
    MessageClass.isCall(message.head)

  /**
    * Deserialize without a `Call` without `Remoting`.
    */
  def deserializeCall(message: ByteString): Call =
    deserializeCall(NoProxyingRemoting, message)

  def deserializeCall(proxying: Proxying, message: ByteString): Call = {
    val _proxying = proxying
    new CallDeserializer with RemotingIUnknownDeserializer {
      val buffer = message.asByteBuffer
      val proxying = _proxying
    }.readCallAndEnd()
  }

  def isCall(byteString: ByteString): Boolean =
    byteString.nonEmpty && MessageClass.isCall(byteString.head)
}
