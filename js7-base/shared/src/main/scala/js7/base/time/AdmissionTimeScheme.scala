package js7.base.time

import io.circe.generic.semiauto.deriveCodec

final case class AdmissionTimeScheme(periods: Seq[AdmissionPeriod])

object AdmissionTimeScheme
{
  implicit val jsonCodec = deriveCodec[AdmissionTimeScheme]

  val always = AdmissionTimeScheme(AlwaysPeriod :: Nil)
}
