package com.sos.jobscheduler.base.generic

import com.sos.jobscheduler.base.circeutils.CirceUtils.RichJson
import com.sos.jobscheduler.base.convert.As
import io.circe.{Decoder, Encoder, Json}
import scala.annotation.tailrec

/**
  * @author Joacim Zschimmer
  */
final case class SecretString(string: String) {
  override def toString = "SecretString"

  /**
    * Special implementation to defend agains timing attacks.
    *
    * @see [[https://codahale.com/a-lesson-in-timing-attacks/]]
    */
  override def equals(other: Any) =
    other match {
      case SecretString(otherString) ⇒ SecretString.equals(string, otherString)
      case _ ⇒ false
    }
}

object SecretString {

  object implicits {
    // Import explicitly, it's secret.

    implicit val JsonEncoder: Encoder[SecretString] = o ⇒ Json.fromString(o.string)
    implicit val JsonDecoder: Decoder[SecretString] = o ⇒ Right(SecretString(o.value.forceString))
  }

  implicit val StringAsSecretString: As[String, SecretString] =
    As(SecretString.apply)

  /**
    * Special implementation to defend agains timing attacks.
    *
    * @see [[https://codahale.com/a-lesson-in-timing-attacks/]]
    */
  def equals(a: String, b: String) = {
    @tailrec def xor(i: Int, result: Int): Int =
      if (i == a.length)
        result
      else
        xor(i + 1, result | (a(i) ^ b(i)))
    a.length == b.length && xor(0, 0) == 0
  }
}
