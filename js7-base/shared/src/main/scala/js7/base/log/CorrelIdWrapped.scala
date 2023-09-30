package js7.base.log

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder}
import js7.base.utils.ScalaUtils.syntax.RichAny

final case class CorrelIdWrapped[+V](correlId: CorrelId, value: V):

  def bindCorrelId[R: CanBindCorrelId](body: V => R): R =
    correlId.orNew.bind(
      body(value))

object CorrelIdWrapped:
  implicit def jsonEncoder[A](implicit A: Encoder.AsObject[A]): Encoder.AsObject[CorrelIdWrapped[A]] =
    o => o.value.asJsonObject
      .pipeIf(o.correlId.nonEmpty)(_
        .add("correlId", o.correlId.asJson))

  implicit def jsonDecoder[A](implicit A: Decoder[A]): Decoder[CorrelIdWrapped[A]] =
    c => for
      correlId <- c.getOrElse[CorrelId]("correlId")(CorrelId.empty)
      a <- c.as[A]
    yield CorrelIdWrapped(correlId, a)
