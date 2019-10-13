package com.sos.jobscheduler.data.filebased

import com.sos.jobscheduler.base.circeutils.CirceUtils.CirceUtilsChecked
import com.sos.jobscheduler.base.generic.GenericString
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.time.Timestamp
import io.circe.{Decoder, Encoder, Json}

/**
  * @author Joacim Zschimmer
  */
final case class VersionId(string: String) extends GenericString
{
  def requireNonAnonymous(): Unit =
    VersionId.checked(string).orThrow

  def isAnonymous = this == VersionId.Anonymous

  override def toString = s"version $string"
}

object VersionId extends GenericString.NonEmpty[VersionId]
{
  val Anonymous = VersionId.unchecked("⊥")

  def generate(isKnown: VersionId => Boolean = _ => false): VersionId = {
    val ts = Timestamp.now.toIsoString
    val short = VersionId("#" + ts.take(19) + ts.drop(19+4)/*tz*/)  // Without milliseconds ".123"
    if (!isKnown(short))
      short
    else {
      val v = VersionId("#" + ts)  // With milliseconds
      if (!isKnown(v))
        v
      else
        Iterator.from(1).map(i => VersionId(s"${v.string}-$i")).collectFirst { case w if !isKnown(w) => w } .get
      }
  }

  override implicit val jsonEncoder: Encoder[VersionId] = o => {
    if (o.isAnonymous) throw new IllegalArgumentException("JSON-serialize VersionId.Anonymous?")
    Json.fromString(o.string)
  }

  override implicit val jsonDecoder: Decoder[VersionId] =
    c => c.as[String] flatMap (o => checked(o).toDecoderResult(c.history))

  def unchecked(string: String) = new VersionId(string)

  override def checked(string: String): Checked[VersionId] =
    for {
      versionId <- super.checked(string) match {
        case Right(VersionId.Anonymous) => Left(Problem.pure("VersionId.Anonymous?"))
        case o => o
      }
    } yield versionId
}
