package js7.proxy.javaapi.data

import io.circe.{Decoder, Encoder}
import io.vavr.control.{Either => VEither}
import js7.base.annotation.javaApi
import js7.base.circeutils.CirceUtils._
import js7.base.problem.Problem
import js7.proxy.javaapi.utils.VavrConversions._

@javaApi
trait JJsonable[A <: JJsonable[A]] extends JavaWrapper
{
  protected def companion: JJsonable.Companion[A]

  final def toJson: String = {
    val companion = this.companion
    val u = underlying.asInstanceOf[companion.Underlying]
    companion.jsonEncoder.apply(u).compactPrint
  }
}

@javaApi
object JJsonable
{
  trait Companion[A <: JJsonable[A]]
  {
    type Underlying = A#Underlying

    def apply(underlying: A#Underlying): A

    implicit def jsonEncoder: Encoder[A#Underlying]
    implicit def jsonDecoder: Decoder[A#Underlying]

    def fromJson(jsonString: String): VEither[Problem, A] =
      io.circe.parser.parse(jsonString).toChecked
        .flatMap(_.as[A#Underlying].toChecked map apply)
        .toVavr
  }
}
