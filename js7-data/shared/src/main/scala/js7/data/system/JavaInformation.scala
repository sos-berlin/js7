package js7.data.system

import io.circe.generic.JsonCodec
import js7.data.system.JavaInformation._

@JsonCodec
final case class JavaInformation(
  version: String,
  availableProcessors: Int,
  memory: Memory,
  systemProperties: Map[String, String])

object JavaInformation {

  @JsonCodec
  final case class Memory(maximum: Long, total: Long, free: Long) {
    def reserve = maximum - total
    def used = total - free
  }
}
