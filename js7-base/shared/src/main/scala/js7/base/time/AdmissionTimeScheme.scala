package js7.base.time

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec

final case class AdmissionTimeScheme(periods: Seq[AdmissionPeriod])

object AdmissionTimeScheme:
  implicit val jsonCodec: Codec.AsObject[AdmissionTimeScheme] = deriveCodec

  val always: AdmissionTimeScheme =
    AdmissionTimeScheme(AlwaysPeriod :: Nil)

  val never: AdmissionTimeScheme =
    new AdmissionTimeScheme(Nil)
