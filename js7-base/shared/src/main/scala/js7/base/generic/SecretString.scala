package js7.base.generic

import io.circe.{Decoder, Encoder, Json}
import java.util.Objects.requireNonNull
import js7.base.circeutils.CirceUtils
import js7.base.convert.As
import js7.base.generic.SecretString._
import scala.annotation.tailrec

/**
  * @author Joacim Zschimmer
  */
final case class SecretString(string: String) {
  requireNonNull(string)

  def provideCharArray[A](body: Array[Char] => A): A = {
    val chars = string.toCharArray
    try body(chars)
    finally for (i <- chars.indices) chars(i) = '\u0000'
  }

  override def toString = "SecretString"

  /**
    * Special implementation to defend agains timing attacks.
    *
    * @see [[https://codahale.com/a-lesson-in-timing-attacks/]]
    */
  override def equals(other: Any) =
    other match {
      case SecretString(otherString) => timingAttackSecureEqual(string, otherString)
      case _ => false
    }
}

object SecretString
{
  val empty = SecretString("")

  object implicits {
    // Import explicitly, it's secret.

    implicit val JsonEncoder: Encoder[SecretString] = o => Json.fromString(o.string)
    implicit val JsonDecoder: Decoder[SecretString] = _.as[String] map SecretString.apply
    val jsonCodec = CirceUtils.circeCodec[SecretString]
  }
  val jsonCodec = implicits.jsonCodec

  implicit val StringAsSecretString: As[String, SecretString] =
    As(SecretString.apply)

  /**
    * Special implementation to defend agains timing attacks.
    *
    * @see [[https://codahale.com/a-lesson-in-timing-attacks/]]
    */
  def timingAttackSecureEqual(a: String, b: String) = {
    @tailrec def xor(i: Int, result: Int): Int =
      if (i == a.length)
        result
      else
        xor(i + 1, result | (a(i) ^ b(i)))
    a.length == b.length && xor(0, 0) == 0
  }
}
