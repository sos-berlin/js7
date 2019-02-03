package com.sos.jobscheduler.data.crypt

import com.sos.jobscheduler.base.utils.Strings.RichString
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, DecodingFailure, JsonObject, ObjectEncoder}

/**
  * @author Joacim Zschimmer
  */
final case class PgpSignature(string: String) extends Signature
{
  override def toString = s"PgpSignature(${string.truncateWithEllipsis(29, showLength = true)})"
}

object PgpSignature
{
  private val SignatureType = "PGP"

  implicit val encoder: ObjectEncoder[PgpSignature] = o ⇒
    JsonObject(
      "TYPE" → SignatureType.asJson,
      "string" → o.string.asJson)

  implicit val decoder: Decoder[PgpSignature] = c ⇒
    for {
      typ ← c.get[String]("TYPE")
      _ ← if (typ == SignatureType) Right(()) else Left(DecodingFailure("Expected TYPE=PGP", Nil))
      string ← c.get[String]("string")
    } yield PgpSignature(string)
}
