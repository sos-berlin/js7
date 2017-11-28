package com.sos.jobscheduler.data.system

import com.sos.jobscheduler.data.system.JavaInformation._
import io.circe.generic.JsonCodec

@JsonCodec
final case class JavaInformation(systemProperties: Map[String, String], memory: Memory)

object JavaInformation {

  @JsonCodec
  final case class Memory(maximum: Long, total: Long, free: Long) {
    def reserve = maximum - total
    def used = total - free
  }
}
