package com.sos.jobscheduler.common.system

import com.sos.jobscheduler.data.system.JavaInformation
import com.sos.jobscheduler.data.system.JavaInformation.Memory

/**
  * @author Joacim Zschimmer
  */
object JavaInformations {

  private val JavaSystemPropertyKeys = List(
    "java.version",
    "java.vendor",
    "os.arch",
    "os.name",
    "os.version")
  private val systemProperties = (for (k ← JavaSystemPropertyKeys; v ← sys.props.get(k)) yield k → v).toMap

  val javaInformation = JavaInformation(
    systemProperties,
    new Memory(
      maximum = sys.runtime.maxMemory,
      total = sys.runtime.totalMemory,
      free = sys.runtime.freeMemory))
}
