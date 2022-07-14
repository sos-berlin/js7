package js7.data.item

import io.circe.{Decoder, Encoder, Json}
import js7.base.annotation.javaApi
import js7.base.circeutils.CirceUtils.CirceUtilsChecked
import js7.base.generic.GenericString
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.time.Timestamp

/**
  * @author Joacim Zschimmer
  */
final case class VersionId(string: String) extends GenericString
{
  def requireNonAnonymous(): Unit =
    VersionId.checked(string).orThrow

  def isAnonymous = this == VersionId.Anonymous

  override def toString = s"Version:$string"
}

object VersionId extends GenericString.NonEmpty[VersionId]
{
  // TODO Restrict VersionId syntax with Js7NameValidator?

  val Anonymous: VersionId = unchecked("⊥")

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
    c => c.as[String].flatMap(o => checked(o).toDecoderResult(c.history))

  protected[item] def unchecked(string: String) = new VersionId(string)

  override def checked(string: String): Checked[VersionId] =
    for {
      versionId <- super.checked(string) match {
        case Right(VersionId.Anonymous) => Left(Problem.pure("VersionId.Anonymous?"))
        case o => o
      }
    } yield versionId

  @javaApi @throws[RuntimeException]("on invalid syntax")
  def of(validVersionId: String): VersionId =
    apply(validVersionId)

  implicit val versionedIdOrdering: Ordering[VersionId] =
    GenericString.ordering[VersionId]
}
