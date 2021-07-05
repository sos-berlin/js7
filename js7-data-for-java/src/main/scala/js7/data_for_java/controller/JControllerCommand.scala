package js7.data_for_java.controller

import io.vavr.control.{Either => VEither}
import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.base.problem.Problem
import js7.data.controller.ControllerCommand
import js7.data_for_java.common.JJsonable
import js7.data_for_java.order.JFreshOrder

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
  @Nonnull
  def addOrder(@Nonnull jFreshOrder: JFreshOrder): JControllerCommand =
    JControllerCommand(ControllerCommand.AddOrder(jFreshOrder.asScala))

  @Nonnull
  override def fromJson(@Nonnull jsonString: String): VEither[Problem, JControllerCommand] =
    super.fromJson(jsonString)

  protected def jsonDecoder = ControllerCommand.jsonCodec
  protected def jsonEncoder = ControllerCommand.jsonCodec
}
