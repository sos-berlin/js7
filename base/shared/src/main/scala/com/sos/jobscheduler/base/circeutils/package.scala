package com.sos.jobscheduler.base

import io.circe.{Decoder, Encoder, ObjectEncoder}

/**
  * @author Joacim Zschimmer
  */
package object circeutils {

  type CirceCodec[A] = Encoder[A] with Decoder[A]
  type CirceObjectCodec[A] = ObjectEncoder[A] with Decoder[A]
}
