package com.sos.jobscheduler.data.system

import com.sos.jobscheduler.data.system.JavaInformation._
import io.circe.generic.JsonCodec

@JsonCodec
final case class JavaInformation(systemProperties: Map[String, String], memory: Memory)

object JavaInformation {
  private val JavaSystemPropertyKeys = List(
    "java.version",
    "java.vendor",
    "os.arch",
    "os.name",
    "os.version")
  private val systemProperties = (for (k ← JavaSystemPropertyKeys; v ← sys.props.get(k)) yield k → v).toMap

  def apply(): JavaInformation = JavaInformation(systemProperties, Memory())

  /** Some constant value for tests. */
  val ForTest = JavaInformation()

  sys.runtime.freeMemory()

  @JsonCodec
  final case class Memory(maximum: Long, total: Long, free: Long) {
    def reserve = maximum - total
    def used = total - free
  }

  object Memory {
    def apply() = new Memory(
      maximum = sys.runtime.maxMemory,
      total = sys.runtime.totalMemory,
      free = sys.runtime.freeMemory)
  }
}
