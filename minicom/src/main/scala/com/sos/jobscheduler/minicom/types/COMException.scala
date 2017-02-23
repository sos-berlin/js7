package com.sos.jobscheduler.minicom.types

/**
 * @author Joacim Zschimmer
 */
class COMException(val hResult: HRESULT, message: String = "") extends RuntimeException {
  override def getMessage = List(hResult.comString, message).mkString(" ")
}
