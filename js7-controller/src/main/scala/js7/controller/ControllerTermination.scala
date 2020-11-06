package js7.controller

sealed trait ControllerTermination

object ControllerTermination
{
  final case class Terminate(restart: Boolean = false) extends ControllerTermination

  final case object Restart extends ControllerTermination
}
