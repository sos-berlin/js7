package js7.base.auth

import js7.base.auth.SessionToken._
import js7.base.generic.SecretString
import js7.base.utils.Assertions.assertThat
import monix.execution.atomic.AtomicInt
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
final case class SessionToken(secret: SecretString)
{
  lazy val number: Long =
    numberOf(secret.string)

  override def toString = short
    //s"SessionToken(${if (number == NoNumber) "?" else number.toString})"

  def short: String =
    numberToShort(number)
}

object SessionToken
{
  val HeaderName = "x-js7-session" /*must be lower case*/
  private val nextNumber = AtomicInt(9)
  private val NoNumber = 0L

  def generateFromSecretString(secretString: SecretString): SessionToken = {
    val nr = nextNumber.incrementAndGet()
    new SessionToken(SecretString(s"$nr,${secretString.string}"))
  }

  def stringToShort(token: String): String =
    numberToShort(numberOf(token))

  private def numberOf(token: String): Long =
    token.indexOf(",") match {
      case -1 => NoNumber
      case n =>
        try token.take(n).toLong
        catch { case NonFatal(_) => NoNumber }
    }

  private def numberToShort(number: Long): String =
    if (number == NoNumber) "▶?" else s"▶$number"
}
