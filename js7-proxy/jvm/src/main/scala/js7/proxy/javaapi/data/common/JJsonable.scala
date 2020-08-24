package js7.proxy.javaapi.data.common

import io.circe.{Decoder, Encoder}
import io.vavr.control.{Either => VEither}
import js7.base.annotation.javaApi
import js7.base.circeutils.CirceUtils._
import js7.base.problem.Problem
import js7.proxy.javaapi.data.common.VavrConverters._

@javaApi
trait JJsonable[A <: JJsonable[A]] extends JavaWrapper
{
  protected def companion: JJsonable.Companion[A]

  final def toJson: String = {
    val companion = this.companion
    val u = asScala.asInstanceOf[companion.Underlying]
    companion.jsonEncoder.apply(u).compactPrint
  }
}

@javaApi
object JJsonable
{
  trait Companion[A <: JJsonable[A]]
  {
    type Underlying = A#AsScala

    def apply(underlying: A#AsScala): A

    implicit def jsonEncoder: Encoder[A#AsScala]
    implicit def jsonDecoder: Decoder[A#AsScala]

    def fromJson(jsonString: String): VEither[Problem, A] =
      io.circe.parser.parse(jsonString).toChecked
        .flatMap(_.as[A#AsScala].toChecked map apply)
        .toVavr
  }
}
