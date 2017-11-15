package com.sos.jobscheduler.master.gui.data.system

import com.sos.jobscheduler.master.gui.data.system.JavaInformation.Memory
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

/**
  * @author Joacim Zschimmer
  */
final case class JavaInformation(
  systemProperties: Map[String, String],
  memory: Memory)

object JavaInformation {

  final case class Memory(maximum: Long, total: Long, free: Long) {
    def reserve = maximum - total
    def used = total - free
  }

  object Memory {
    implicit val jsonDecoder: Decoder[Memory] = deriveDecoder[Memory]
  }

  implicit val jsonDecoder: Decoder[JavaInformation] = deriveDecoder[JavaInformation]
}
