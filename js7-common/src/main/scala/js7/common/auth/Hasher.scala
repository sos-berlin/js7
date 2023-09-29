package js7.common.auth

import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest
import js7.base.log.Logger
import js7.base.utils.ScalaUtils.*
import js7.common.auth.Hasher.*

final class Hasher(algorithm: String) extends (String => String)
{
  private val prototype = MessageDigest.getInstance(algorithm)
  private var isCloneable = true

  def apply(string: String) =
    bytesToHex(cloneMessageDigest().digest(string.getBytes(UTF_8)))

  private def cloneMessageDigest() =
    if isCloneable then {
      try prototype.clone().asInstanceOf[MessageDigest]
      catch { case _: CloneNotSupportedException =>
        isCloneable = false
        logger.debug(s"$algorithm MessageDigest is not cloneable")
        newMessageDigest()
      }
    } else
      newMessageDigest()

  private def newMessageDigest() =
    MessageDigest.getInstance(algorithm)

  override def toString = algorithm
}

object Hasher
{
  lazy val sha512 = new Hasher("SHA-512")
  private val logger = Logger[this.type]
}
