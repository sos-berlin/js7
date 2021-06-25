package js7.data.workflow

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, JsonObject}
import js7.base.utils.ScalaUtils.syntax.RichBoolean
import js7.base.utils.typeclasses.IsEmpty
import js7.data.job.JobResourcePath
import js7.data.workflow.OrderRequirements._

final case class OrderRequirements(parameters: WorkflowParameters)
{
  def isEmpty = this == default

  def referencedJobResourcePaths: Iterable[JobResourcePath] =
    parameters.referencedJobResourcePaths
}

object OrderRequirements
{
  val default = OrderRequirements(WorkflowParameters.default)
  implicit val orderRequirementsIsEmpty = IsEmpty[OrderRequirements](_.isEmpty)

  implicit val jsonEncoder: Encoder.AsObject[OrderRequirements] =
    o => JsonObject(
      "parameters" -> (o.parameters.nameToParameter.nonEmpty ? o.parameters).asJson,
      "allowUndeclared" -> (o.parameters.allowUndeclared ? true).asJson)

  implicit val jsonDecoder: Decoder[OrderRequirements] =
    c => for {
      parameters <- c.getOrElse[WorkflowParameters]("parameters")(WorkflowParameters.default)
      allowUndeclared <- c.getOrElse[Boolean]("allowUndeclared")(false)
    } yield OrderRequirements(parameters.copy(allowUndeclared = allowUndeclared))
}
