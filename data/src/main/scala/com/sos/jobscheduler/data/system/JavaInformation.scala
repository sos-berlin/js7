package com.sos.scheduler.engine.data.system

import spray.json.DefaultJsonProtocol._
import JavaInformation._

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

  final case class Memory(maximum: Long, total: Long, free: Long) {
    def reserve = maximum - total
    def used = total - free
  }

  object Memory {
    implicit val myJsonFormat = jsonFormat3(apply)
    def apply() = new Memory(
      maximum = sys.runtime.maxMemory,
      total = sys.runtime.totalMemory,
      free = sys.runtime.freeMemory)
  }

  implicit val MyJsonFormat = jsonFormat2(apply)
}
