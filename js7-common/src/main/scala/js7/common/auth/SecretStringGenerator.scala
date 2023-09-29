package js7.common.auth

import java.security.SecureRandom
import java.util.Base64
import js7.base.generic.SecretString

/**
  * @author Joacim Zschimmer
  */
object SecretStringGenerator:
  private val random = new SecureRandom
  private val ByteCount = 18  // 144 bits (a UUID has 128 bits). For base64, a multiple of 3 bytes is good.
  private val toUrlBase64 = Base64.getUrlEncoder.withoutPadding.encodeToString _

  /** Returns a base64-encoded random SecretString. */
  def newSecretString(): SecretString =
    SecretString(randomString())

  /** Returns a base64-encoded random String. */
  def randomString(): String =
    val bytes = new Array[Byte](ByteCount)
    random.nextBytes(bytes)
    toUrlBase64(bytes)
