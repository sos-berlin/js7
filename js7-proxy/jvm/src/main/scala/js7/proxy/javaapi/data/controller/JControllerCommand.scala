package js7.proxy.javaapi.data.controller

import io.circe.{Decoder, Encoder}
import io.vavr.control.{Either => VEither}
import js7.base.annotation.javaApi
import js7.base.problem.Problem
import js7.controller.data.ControllerCommand
import js7.proxy.javaapi.data.common.JJsonable
import js7.proxy.javaapi.data.order.JFreshOrder

@javaApi
final case class JControllerCommand(asScala: ControllerCommand)
extends JJsonable[JControllerCommand]
{
  protected type AsScala = ControllerCommand

  protected def companion = JControllerCommand
}

@javaApi
object JControllerCommand extends JJsonable.Companion[JControllerCommand]
{
  def addOrder(jFreshOrder: JFreshOrder): JControllerCommand =
    JControllerCommand(ControllerCommand.AddOrder(jFreshOrder.asScala))

  override def fromJson(jsonString: String): VEither[Problem, JControllerCommand] =
    super.fromJson(jsonString)

  protected def jsonDecoder = ControllerCommand.jsonCodec
  protected def jsonEncoder = ControllerCommand.jsonCodec
}
