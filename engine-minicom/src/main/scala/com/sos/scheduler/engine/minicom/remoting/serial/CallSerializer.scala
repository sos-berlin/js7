package com.sos.scheduler.engine.minicom.remoting.serial

import akka.util.ByteString
import com.sos.scheduler.engine.minicom.remoting.calls._
import com.sos.scheduler.engine.minicom.remoting.proxy.ProxyRegister
import com.sos.scheduler.engine.minicom.remoting.serial.CallDeserializer._

/**
 * @author Joacim Zschimmer
 */
private final class CallSerializer(protected val proxyRegister: ProxyRegister) extends IDispatchSerializer {

  def writeCall(call: Call): Unit =
    call match {
      case call: SessionCall ⇒
        writeByte(MessageClass.Session)
        writeSessionCall(call)
      case call: ObjectCall ⇒
        writeByte(MessageClass.Object)
        writeInt64(call.proxyId.value)
        writeObjectCall(call)
      case KeepAliveCall ⇒
        writeByte(MessageClass.KeepAlive)
    }

  private def writeSessionCall(call: SessionCall) =
    call match {
      case CreateInstanceCall(clsid, outer, context, iids) ⇒
        writeUUID(clsid.uuid)
        writeNull() // outer
        writeInt32(0) // context
        writeInt32(iids.size)
        for (o ← iids) writeUUID(o.uuid)
    }

  private def writeObjectCall(call: ObjectCall) =
    call match {
      //      case ReleaseCall(proxyInvocable) ⇒   // TODO
      //        writeByte(MessageClass.Object)
      //        writeInvocable(proxyInvocable.id)
      //        writeByte(MessageCommand.Release)

      case QueryInterfaceCall(proxyId, iid) ⇒
        writeByte(MessageCommand.GetIDsOfNames)
        writeInt64(proxyId.value)
        writeUUID(iid.uuid)

      case InvokeCall(proxyId, dispatchId, iid, dispatchTypes, arguments, namedArguments) ⇒
        writeByte(MessageCommand.Invoke)
        writeInt32(dispatchId.value)
        writeUUID(iid.uuid)
        writeInt32(0)  // localeId
        writeInt32((dispatchTypes map { _.value }).fold(0) { _ | _ })
        writeInt32(arguments.size + namedArguments.size)
        writeInt32(namedArguments.size)
        for (a ← namedArguments.reverseIterator) writeInt32(a._1.value)
        for (a ← (namedArguments.reverseIterator map { _._2 }) ++ arguments.reverseIterator) writeVariant(a)

      case GetIDsOfNamesCall(proxyId, iid, localeId, names) ⇒
        writeByte(MessageCommand.GetIDsOfNames)
        writeUUID(iid.uuid)
        writeInt32(localeId)
        writeInt32(names.size)
        names foreach writeString

      case _: ReleaseCall | _: CallCall ⇒ throw new UnsupportedOperationException(call.getClass.getSimpleName)
    }
}

private[remoting] object CallSerializer {
  /**
   * @return (Array, length)
   */
  def serializeCall(proxyRegister: ProxyRegister, call: Call): ByteString = {
    val serializer = new CallSerializer(proxyRegister)
    serializer.writeCall(call)
    serializer.toByteString
  }
}
