package com.sos.jobscheduler.data.filebased

import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class SignedRepoObject(message: String, signatureType: String, signature: String)
