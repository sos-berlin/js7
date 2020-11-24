package js7.proxy.javaapi.data.order

import io.vavr.control.{Either => VEither}
import js7.base.annotation.javaApi
import js7.base.problem.Problem
import js7.data.order.Outcome
import js7.data.value.Value
import js7.proxy.javaapi.data.common.JJsonable
import scala.jdk.CollectionConverters._

final case class JOutcome(asScala: Outcome) extends JJsonable[JOutcome]
{
  type AsScala = Outcome
  def companion = JOutcome
}

object JOutcome extends JJsonable.Companion[JOutcome]
{
  @javaApi
  val succeeded = JOutcome(Outcome.succeeded)

  @javaApi
  def succeeded(namedValues: java.util.Map[String, Value]) =
    JOutcome(Outcome.Succeeded(namedValues.asScala.toMap))

  @javaApi
  val failed = JOutcome(Outcome.failed)

  @javaApi
  def failed(namedValues: java.util.Map[String, Value]) =
    JOutcome(Outcome.Failed(namedValues.asScala.toMap))

  override def fromJson(jsonString: String): VEither[Problem, JOutcome] =
    super.fromJson(jsonString)

  protected def jsonEncoder = Outcome.jsonCodec
  protected def jsonDecoder = Outcome.jsonCodec
}
