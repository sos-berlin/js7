package js7.base.auth

import js7.base.auth.SessionToken.*
import js7.base.generic.SecretString
import js7.base.utils.Atomic
import scala.util.control.NonFatal

/** Identifies a session.
  *
  * OWASP calls this Session ID.
  */
final case class SessionToken(secret: SecretString):

  lazy val number: Long =
    numberOf(secret.string)

  override def toString = short

  def short: String =
    numberToShort(number)


object SessionToken:
  private val nextNumber = Atomic(0)
  private val NoNumber = 0L
  private val NoNumberShort = "Session:?"

  def generateFromSecretString(secretString: SecretString): SessionToken =
    val nr = nextNumber.incrementAndGet()
    new SessionToken(SecretString(s"$nr,${secretString.string}"))

  def stringToShort(token: String): String =
    numberToShort(numberOf(token))

  private def numberOf(token: String): Long =
    token.indexOf(",") match
      case -1 => NoNumber
      case n =>
        try token.take(n).toLong
        catch { case NonFatal(_) => NoNumber }

  private def numberToShort(number: Long): String =
    if number == NoNumber then NoNumberShort else s"Session:$number"
