package com.sos.jobscheduler.common.utils

/**
  * @author Joacim Zschimmer
  */
object ByteUnits {

  def toKBGB(size: Long): String =
    size match {
      case _ if size < 0 ⇒ size + "bytes"
      case _ if size == 0  ⇒ "0KB"
      case _ if size < 1000  ⇒ "<1KB"
      case _ if size < 1000000 ⇒ size / 1000 + "KB"
      case _ if size < 1000000000 ⇒ size / 1000000 + "MB"
      case _ ⇒ size / 1000000000 + "GB"
    }

  def toMB(size: Long): String =
    size match {
      case _ if size < 0 ⇒ toKBGB(size)
      case _ if size == 0 ⇒ "0MB"
      case _ if size < 1000 * 1000 ⇒ "<1MB"
      case _ ⇒ toKBGB(size)
    }
}
