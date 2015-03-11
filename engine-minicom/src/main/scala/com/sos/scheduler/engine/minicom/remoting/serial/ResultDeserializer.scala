package com.sos.scheduler.engine.minicom.remoting.serial

import com.sos.scheduler.engine.minicom.idispatch.DISPID
import com.sos.scheduler.engine.minicom.remoting.calls.{CreateInstanceResult, GetIDsOfNamesResult, InvokeResult, MessageClass}
import com.sos.scheduler.engine.minicom.remoting.serial.ResultDeserializer._
import com.sos.scheduler.engine.minicom.types.HRESULT._
import com.sos.scheduler.engine.minicom.types.{COMException, HRESULT}
import java.nio.ByteBuffer
import org.scalactic.Requirements._
import scala.collection.mutable

/**
 * @author Joacim Zschimmer
 */
private[remoting] final class ResultDeserializer(
  protected val remoting: ServerRemoting,
  protected val buffer: ByteBuffer)
extends IUnknownDeserializer {

  def readCreateInstanceResult(): CreateInstanceResult = {
    readAnswerHeader()
    require(HRESULT(readInt32()) == S_OK)
    CreateInstanceResult(readInvocableOption().get)
  }

  def readGetIDsOfNamesResult(): GetIDsOfNamesResult = {
    readAnswerHeader()
    val dispids = Vector.fill(readInt32()) { DISPID(readInt32()) }
    GetIDsOfNamesResult(dispids)
  }

  def readInvokeResult(): InvokeResult = {
    readAnswerHeader()
    InvokeResult(readVariant())
  }

  private def readAnswerHeader(): Unit = {
    readByte() match {
      case MessageClass.Answer ⇒
        val hr = HRESULT(readInt32())
        if (hr.isError) throw new COMException(hr)
      case MessageClass.Error ⇒
        val strings = mutable.Buffer[String]()
        for (_ ← 1 to 3) readString() match {
          case KeyValueRegex("name", v) ⇒ strings += v
          case KeyValueRegex("code", v) ⇒ strings += v
          case KeyValueRegex("what", v) ⇒ strings += v
          case v ⇒ strings += v
        }
        throw new COMException(DISP_E_EXCEPTION, strings mkString " ")
    }
  }
}

object ResultDeserializer {
  private val KeyValueRegex = "([a-z]+)=(.*)".r
  private val ComErrorRegex = "COM-[0-9a-fA-F]{8}".r
}
