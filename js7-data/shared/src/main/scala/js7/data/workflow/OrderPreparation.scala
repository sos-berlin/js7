package js7.data.workflow

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject}
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import js7.base.utils.typeclasses.IsEmpty
import js7.data.job.JobResourcePath
import js7.data.workflow.OrderPreparation._

final case class OrderPreparation(parameters: OrderParameters)
{
  def isEmpty = this == default

  def referencedJobResourcePaths: Iterable[JobResourcePath] =
    parameters.referencedJobResourcePaths
}

object OrderPreparation
{
  val default = OrderPreparation(OrderParameters.default)
  implicit val orderPreparationIsEmpty = IsEmpty[OrderPreparation](_.isEmpty)

  implicit val jsonEncoder: Encoder.AsObject[OrderPreparation] =
    o => JsonObject(
      "parameters" -> (o.parameters.nameToParameter.nonEmpty ? o.parameters).asJson,
      "allowUndeclared" -> (o.parameters.allowUndeclared ? true).asJson)

  implicit val jsonDecoder: Decoder[OrderPreparation] =
    c => for {
      parameters <- c.getOrElse[OrderParameters]("parameters")(OrderParameters.default)
      allowUndeclared <- c.getOrElse[Boolean]("allowUndeclared")(false)
    } yield OrderPreparation(parameters.copy(allowUndeclared = allowUndeclared))
}
