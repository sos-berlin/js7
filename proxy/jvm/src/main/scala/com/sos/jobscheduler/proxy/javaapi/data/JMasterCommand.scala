package js7.proxy.javaapi.data

import js7.base.annotation.javaApi
import js7.base.problem.Problem
import js7.master.data.MasterCommand
import io.circe.{Decoder, Encoder}
import io.vavr.control.{Either => VEither}

@javaApi
final case class JMasterCommand(underlying: MasterCommand)
extends JJsonable[JMasterCommand]
{
  protected type Underlying = MasterCommand

  protected def companion = JMasterCommand
}

@javaApi
object JMasterCommand extends JJsonable.Companion[JMasterCommand]
{
  def addOrder(jFreshOrder: JFreshOrder): JMasterCommand =
    JMasterCommand(MasterCommand.AddOrder(jFreshOrder.underlying))

  override def fromJson(jsonString: String): VEither[Problem, JMasterCommand] =
    super.fromJson(jsonString)

  def jsonDecoder = implicitly[Decoder[MasterCommand]]

  def jsonEncoder = implicitly[Encoder[MasterCommand]]
}
