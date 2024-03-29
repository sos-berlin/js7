package js7.data_for_java.common

import io.circe.{Decoder, Encoder}
import io.vavr.control.Either as VEither
import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.base.circeutils.CirceUtils.*
import js7.base.problem.Problem
import js7.data_for_java.vavr.VavrConverters.*

@javaApi
trait JJsonable[A <: JJsonable[A]] extends JavaWrapper:

  protected def companion: JJsonable.Companion[A]

  final def toJson: String =
    val companion = this.companion
    val u = asScala.asInstanceOf[companion.AsScala]
    companion.myJsonEncoder.apply(u).compactPrint


@javaApi
object JJsonable:
  trait Companion[A <: JJsonable[A]]:
    /** Must be the same type as JJsonable#AsScala. */
    type AsScala

    def apply(underlying: AsScala): A

    private[JJsonable] def myJsonEncoder = jsonEncoder

    protected def jsonEncoder: Encoder[AsScala]
    protected def jsonDecoder: Decoder[AsScala]

    @Nonnull
    def fromJson(@Nonnull jsonString: String): VEither[Problem, A] =
      io.circe.parser.parse(jsonString).toChecked
        .flatMap(o => jsonDecoder.decodeJson(o).toChecked map apply)
        .toVavr
