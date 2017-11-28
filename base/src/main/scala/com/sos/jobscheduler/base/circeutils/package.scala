package com.sos.jobscheduler.base

import io.circe.generic.decoding.DerivedDecoder
import io.circe.generic.encoding.DerivedObjectEncoder
import io.circe.{Decoder, Encoder, HCursor, Json, JsonObject}
import shapeless.Lazy

/**
  * @author Joacim Zschimmer
  */
package object circeutils {

  type CirceCodec[A] = Encoder[A] with Decoder[A]
  type CirceObjectCodec[A] = Encoder[A] with Decoder[A]
}
