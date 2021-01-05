package js7.base.auth

import js7.base.auth.SessionToken._
import js7.base.generic.SecretString
import monix.execution.atomic.AtomicInt
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
final case class SessionToken(secret: SecretString)
{
  private lazy val number: Long =
    secret.string.indexOf(",") match {
      case -1 => NoNumber
      case n =>
        try secret.string.take(n).toLong
        catch { case NonFatal(_) => NoNumber }
    }

  override def toString = short
    //s"SessionToken(${if (number == NoNumber) "?" else number.toString})"

  def short: String =
    if (number == NoNumber) "SessionToken" else s"▶$number"
}

object SessionToken
{
  val HeaderName = "X-JS7-Session"
  private val nextNumber = AtomicInt(9)
  private val NoNumber = 0L

  def generateFromSecretString(secretString: SecretString): SessionToken = {
    val nr = nextNumber.incrementAndGet()
    new SessionToken(SecretString(s"$nr,${secretString.string}"))
  }
}
