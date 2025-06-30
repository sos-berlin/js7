package js7.base.time

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject}
import js7.base.time.SchemeRestriction.Unrestricted
import scala.annotation.targetName

final case class AdmissionTimeScheme(restrictedSchemes: Seq[RestrictedScheme])


object AdmissionTimeScheme:

  private val standardEncoder = deriveEncoder[AdmissionTimeScheme]
  private val standardDecoder = deriveDecoder[AdmissionTimeScheme]

  given Encoder.AsObject[AdmissionTimeScheme] =
    o =>
      if o.restrictedSchemes.sizeIs == 1
        && (o.restrictedSchemes.head.restriction eq Unrestricted)
      then
        JsonObject("periods" -> o.restrictedSchemes.head.periods.asJson)
      else
        standardEncoder.encodeObject(o)


  given Decoder[AdmissionTimeScheme] = c =>
    c.value.asObject.flatMap(_("periods")).map: periods =>
      // COMPATIBLE with v2.7
      periods.as[Seq[AdmissionPeriod]].map: periods =>
        AdmissionTimeScheme(periods)
    .getOrElse:
      standardDecoder(c)

  val always: AdmissionTimeScheme =
    AdmissionTimeScheme(AlwaysPeriod :: Nil)

  val never: AdmissionTimeScheme =
    new AdmissionTimeScheme(Nil)

  @targetName("fromPeriods")
  def apply(periods: Seq[AdmissionPeriod]): AdmissionTimeScheme =
    new AdmissionTimeScheme(
      new RestrictedScheme(periods) :: Nil)
