package js7.data.workflow

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject}
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import js7.base.utils.typeclasses.IsEmpty
import js7.data.job.JobResourcePath
import js7.data.workflow.OrderPreparation.*
import scala.collection.View

final case class OrderPreparation(parameterList: OrderParameterList)
{
  def isEmpty = this == default

  def referencedJobResourcePaths: View[JobResourcePath] =
    parameterList.referencedJobResourcePaths
}

object OrderPreparation
{
  val default = OrderPreparation(OrderParameterList.default)

  implicit val orderPreparationIsEmpty: IsEmpty[OrderPreparation] =
    IsEmpty(_.isEmpty)

  implicit val jsonEncoder: Encoder.AsObject[OrderPreparation] =
    o => JsonObject(
      "parameters" -> (o.parameterList.nameToParameter.nonEmpty ? o.parameterList).asJson,
      "allowUndeclared" -> (o.parameterList.allowUndeclared ? true).asJson)

  implicit val jsonDecoder: Decoder[OrderPreparation] =
    c => for
      parameters <- c.getOrElse[OrderParameterList]("parameters")(OrderParameterList.default)
      allowUndeclared <- c.getOrElse[Boolean]("allowUndeclared")(false)
    yield OrderPreparation(parameters.copy(allowUndeclared = allowUndeclared))
}
