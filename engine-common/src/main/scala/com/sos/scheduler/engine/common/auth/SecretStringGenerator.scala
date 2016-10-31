package com.sos.scheduler.engine.common.auth

import com.sos.scheduler.engine.base.generic.SecretString
import java.security.SecureRandom
import java.util.Base64

/**
  * @author Joacim Zschimmer
  */
object SecretStringGenerator {
  private val random = new SecureRandom
  private val ByteCount = 18  // 144 bits (a UUID has 128 bits). For base64, a multiple of 3 bytes is good.
  private val toUrlBase64 = Base64.getUrlEncoder.encodeToString _

  def newSecretString(): SecretString = SecretString({
    val bytes = new Array[Byte](ByteCount)
    random.nextBytes(bytes)
    toUrlBase64(bytes) stripSuffix "="
  })
}
