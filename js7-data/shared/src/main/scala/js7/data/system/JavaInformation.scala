package js7.data.system

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import js7.data.system.JavaInformation.*

final case class JavaInformation(
  version: String,
  availableProcessors: Int,
  memory: Memory,
  systemProperties: Map[String, String])


object JavaInformation:

  final case class Memory(maximum: Long, total: Long, free: Long):
    def reserve = maximum - total
    def used = total - free
  object Memory:
    implicit val jsonCodec: Codec.AsObject[Memory] = deriveCodec

  implicit val jsonCodec: Codec.AsObject[JavaInformation] = deriveCodec
