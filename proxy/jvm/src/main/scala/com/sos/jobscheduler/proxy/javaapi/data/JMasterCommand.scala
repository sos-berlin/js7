package com.sos.jobscheduler.proxy.javaapi.data

import com.sos.jobscheduler.base.annotation.javaApi
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.master.data.MasterCommand
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
