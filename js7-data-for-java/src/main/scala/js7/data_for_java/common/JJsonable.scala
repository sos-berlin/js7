package js7.data_for_java.common

import io.circe.{Decoder, Encoder}
import io.vavr.control.{Either => VEither}
import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.base.circeutils.CirceUtils._
import js7.base.problem.Problem
import js7.data_for_java.vavr.VavrConverters._

@javaApi
trait JJsonable[A <: JJsonable[A]] extends JavaWrapper
{
  protected def companion: JJsonable.Companion[A]

  final def toJson: String = {
    val companion = this.companion
    val u = asScala.asInstanceOf[companion.AsScala]
    companion.myJsonEncoder.apply(u).compactPrint
  }
}

@javaApi
object JJsonable
{
  trait Companion[A <: JJsonable[A]]
  {
    type AsScala = A#AsScala

    def apply(underlying: A#AsScala): A

    private[JJsonable] def myJsonEncoder = jsonEncoder

    protected def jsonEncoder: Encoder[A#AsScala]
    protected def jsonDecoder: Decoder[A#AsScala]

    @Nonnull
    def fromJson(@Nonnull jsonString: String): VEither[Problem, A] =
      io.circe.parser.parse(jsonString).toChecked
        .flatMap(o => jsonDecoder.decodeJson(o).toChecked map apply)
        .toVavr
  }
}
