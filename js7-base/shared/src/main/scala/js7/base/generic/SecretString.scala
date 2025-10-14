package js7.base.generic

import io.circe.{Codec, Decoder, Encoder, Json}
import java.util.Objects.requireNonNull
import js7.base.convert.As
import js7.base.generic.SecretString.*
import scala.annotation.tailrec

/**
  * @author Joacim Zschimmer
  */
final case class SecretString(string: String):

  requireNonNull(string)

  def provideCharArray[A](body: Array[Char] => A): A =
    val chars = string.toCharArray
    try body(chars)
    finally for i <- chars.indices do chars(i) = '\u0000'

  def isEmpty: Boolean =
    string.isEmpty

  def nonEmpty: Boolean =
    string.nonEmpty

  override def toString = "Secret"

  /**
    * Special implementation to defend agains timing attacks.
    *
    * @see [[https://codahale.com/a-lesson-in-timing-attacks/]]
    */
  override def equals(other: Any): Boolean =
    other match
      case SecretString(otherString) => timingAttackSecureEqual(string, otherString)
      case _ => false


object SecretString:
  val empty: SecretString = SecretString("")

  /** Import explicitly, the JSON Encoder reveals the secret. */
  val jsonCodec: Codec[SecretString] = Codec.from(
    decodeA = _.as[String] map SecretString.apply,
    encodeA = o => Json.fromString(o.string))

  implicit val StringAsSecretString: As[String, SecretString] =
    As(SecretString.apply)

  /**
    * Special implementation to defend against timing attacks.
    *
    * @see [[https://en.wikipedia.org/wiki/Timing_attack]]
    */
  def timingAttackSecureEqual(a: String, b: String): Boolean =
    @tailrec def xor(i: Int, result: Int): Int =
      if i == a.length then
        result
      else
        xor(i + 1, result | (a(i) ^ b(i)))

    a.length == b.length && xor(0, 0) == 0
