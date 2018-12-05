package com.sos.jobscheduler.data.filebased

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.circeutils.CirceUtils.CirceUtilsChecked
import com.sos.jobscheduler.base.generic.GenericString
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import io.circe.{Decoder, Encoder, Json}

/**
  * @author Joacim Zschimmer
  */
final case class VersionId(string: String) extends GenericString
{
  VersionId.check(string).orThrow

  def requireNonAnonymous(): Unit =
    VersionId.checked(string).orThrow

  def isAnonymous = this == VersionId.Anonymous

  override def toString = s"version $string"
}

object VersionId extends GenericString.Checked_[VersionId]
{
  val Anonymous = VersionId.unchecked("⊥")

  override implicit val jsonEncoder: Encoder[VersionId] = o ⇒ {
    if (o.isAnonymous) throw new IllegalArgumentException("JSON-serialize VersionId.Anonymous?")
    Json.fromString(o.string)
  }

  override implicit val jsonDecoder: Decoder[VersionId] =
    _.as[String] flatMap (o ⇒ checked(o).toDecoderResult)

  def unchecked(string: String) = new VersionId(string)

  override def checked(string: String): Checked[VersionId] =
    for {
      _ ← check(string)
      versionId ← super.checked(string) match {
        case Valid(VersionId.Anonymous) ⇒ Invalid(Problem.pure("VersionId.Anonymous?"))
        case o ⇒ o
      }
    } yield versionId

  def check(string: String): Checked[Unit] =
    if (string.isEmpty)
      Problem("Empty VersionId?")
    else
      Checked.unit
}
