package com.sos.scheduler.engine.minicom.remoting.serial

import akka.util.ByteString
import com.sos.scheduler.engine.minicom.idispatch.{DISPID, DispatchType}
import com.sos.scheduler.engine.minicom.remoting.calls._
import com.sos.scheduler.engine.minicom.remoting.serial.CallDeserializer._
import com.sos.scheduler.engine.minicom.types.{CLSID, IID}
import java.nio.ByteBuffer
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

  final def readCall(): Call =
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
    val CreateInstance  = 'C'.toByte
    val Release         = 'R'.toByte
    val QueryInterface  = 'Q'.toByte
    val GetIDsOfNames   = 'G'.toByte
    val Invoke          = 'I'.toByte
    val Call            = 'A'.toByte
  }

  def isCallCall(message: ByteString): Boolean =
    message.head == MessageClass.Object && message(1 + 8) == MessageCommand.Call

  def isReleaseCall(message: ByteString): Boolean =
    message.head == MessageClass.Object && message(1 + 8) == MessageCommand.Release

  def deserializeCall(remoting: ServerRemoting, message: ByteString) = {
    val _remoting = remoting
    new CallDeserializer with RemotingIUnknownDeserializer {
      val buffer = message.asByteBuffer
      val remoting = _remoting
    }.readCallAndEnd()
  }

  def deserializeCall(message: ByteString) = new SimpleCallDeserializer(message.asByteBuffer).readCallAndEnd()

  private class SimpleCallDeserializer(protected val buffer: ByteBuffer) extends CallDeserializer with RemotingIUnknownDeserializer {
    val remoting = ServerRemoting.Dummy
  }
}
