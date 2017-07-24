package com.sos.jobscheduler.shared.event.journal

/**
  * @author Joacim Zschimmer
  */
private[journal] object Utils {

  def toMB(size: Long): String = size match {
    case _ if size < 1000 * 1000 ⇒ "<1MB"
    case _ ⇒ (size + 999999) / (1000 * 1000) + "MB"
  }
}
