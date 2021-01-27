package js7.data.workflow

import io.circe.generic.semiauto.deriveCodec
import js7.base.problem.Checked
import js7.data.value.{NamedValues, Value}

final case class OrderRequirements(
  parameters: Option[WorkflowParameters])
{
  def isEmpty = parameters.isEmpty

  def nonEmpty = !isEmpty

  def checkArguments(namedValues: NamedValues): Checked[Unit] =
    parameters.fold(Checked.unit)(p => p.checkNamedValues(namedValues))

  def defaultArgument(name: String): Option[Value] =
    parameters
      .flatMap(_.nameToParameter.get(name))
      .flatMap(_.default)
}

object OrderRequirements
{
  val empty = OrderRequirements(None)
  implicit val jsonCodec = deriveCodec[OrderRequirements]
}
