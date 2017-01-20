package com.sos.scheduler.engine.minicom.remoting.serial

import com.sos.scheduler.engine.minicom.idispatch.DISPID
import com.sos.scheduler.engine.minicom.remoting.calls._
import com.sos.scheduler.engine.minicom.remoting.serial.ResultDeserializer._
import com.sos.scheduler.engine.minicom.types.HRESULT._
import com.sos.scheduler.engine.minicom.types.{COMException, HRESULT}
import java.util.Objects.requireNonNull
import org.scalactic.Requirements._
import scala.collection.mutable

/**
 * @author Joacim Zschimmer
 */
private[remoting] trait ResultDeserializer
extends VariantDeserializer  {

  def readCreateInstanceResult(): CreateInstanceResult = {
    readAnswerHeader()
    require(HRESULT(readInt32()) == S_OK)
    val iUnknown = requireNonNull(readIUnknownOrNull())
    requireEndOfMessage()
    CreateInstanceResult(iUnknown)
  }

  def readGetIDsOfNamesResult(n: Int): GetIDsOfNamesResult = {
    readAnswerHeader()
    val dispids = Vector.fill(n) { DISPID(readInt32()) }
    requireEndOfMessage()
    GetIDsOfNamesResult(dispids)
  }

  def readInvokeResult(): InvokeResult = {
    val hr = readAnswerHeaderHRESULT()
    if (hr.isError) {
      readInt32()  // argErr
      val message = readExcepInfo().toString
      throw new COMException(hr, message)
    }
    val result = readVariant()
    requireEndOfMessage()
    InvokeResult(result)
  }

  def readEmptyResult(): EmptyResult.type = {
    // Response to KeepAlive
    require(readByte() == MessageClass.Answer)
    requireEndOfMessage()
    EmptyResult
  }

  private def readAnswerHeader(): Unit = {
    val hr = readAnswerHeaderHRESULT()
    if (hr.isError) throw new COMException(hr)
  }

  private def readAnswerHeaderHRESULT(): HRESULT =
    readByte() match {
      case MessageClass.Answer ⇒
        HRESULT(readInt32())

      case MessageClass.Error ⇒
        val others = mutable.Buffer[String]()
        var name, code, what: String = ""
        for (_ ← 1 to 3) readString() match {
          case KeyValueRegex("name", v) ⇒ name = v
          case KeyValueRegex("code", v) ⇒ code = v
          case KeyValueRegex("what", v) ⇒ what = v
          case v ⇒ others += v
        }
        val strings = mutable.Buffer[String]()
        // Remove redundancy
        if (!code.startsWith(name)) strings += name
        if (!what.startsWith(code)) strings += code
        val m = DISP_E_EXCEPTION.comString
        strings += (if (what.startsWith(m)) what.substring(m.length).trim else what)
        strings ++= others
        throw new COMException(DISP_E_EXCEPTION, strings mkString " ")
    }
}

object ResultDeserializer {
  private val KeyValueRegex = "([a-z]+)=(?s)(.*)".r
}
