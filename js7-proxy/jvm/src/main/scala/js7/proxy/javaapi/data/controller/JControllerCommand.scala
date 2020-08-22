package js7.proxy.javaapi.data.controller

import io.circe.{Decoder, Encoder}
import io.vavr.control.{Either => VEither}
import js7.base.annotation.javaApi
import js7.base.problem.Problem
import js7.controller.data.ControllerCommand
import js7.proxy.javaapi.data.common.JJsonable
import js7.proxy.javaapi.data.order.JFreshOrder

@javaApi
final case class JControllerCommand(underlying: ControllerCommand)
extends JJsonable[JControllerCommand]
{
  protected type Underlying = ControllerCommand

  protected def companion = JControllerCommand
}

@javaApi
object JControllerCommand extends JJsonable.Companion[JControllerCommand]
{
  def addOrder(jFreshOrder: JFreshOrder): JControllerCommand =
    JControllerCommand(ControllerCommand.AddOrder(jFreshOrder.underlying))

  override def fromJson(jsonString: String): VEither[Problem, JControllerCommand] =
    super.fromJson(jsonString)

  def jsonDecoder = implicitly[Decoder[ControllerCommand]]

  def jsonEncoder = implicitly[Encoder[ControllerCommand]]
}
