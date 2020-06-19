package js7.controller

private[controller] sealed trait ControllerTermination

private[controller] object ControllerTermination
{
  final case class Terminate(restart: Boolean = false) extends ControllerTermination

  final case object Restart extends ControllerTermination
}
