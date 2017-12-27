package com.sos.jobscheduler.base

import io.circe.{Decoder, Encoder}

/**
  * @author Joacim Zschimmer
  */
package object circeutils {

  type CirceCodec[A] = Encoder[A] with Decoder[A]
}
