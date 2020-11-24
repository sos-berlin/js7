package js7.proxy.javaapi.data.order

import io.vavr.control.{Either => VEither}
import js7.base.annotation.javaApi
import js7.base.problem.Problem
import js7.data.order.HistoricOutcome
import js7.proxy.javaapi.data.common.JJsonable
import js7.proxy.javaapi.data.workflow.position.JPosition

/** HistoricOutcome in a Workflow. */
final case class JHistoricOutcome(asScala: HistoricOutcome)
extends JJsonable[JHistoricOutcome]
{
  protected type AsScala = HistoricOutcome
  protected def companion = JHistoricOutcome
}

object JHistoricOutcome extends JJsonable.Companion[JHistoricOutcome]
{
  @javaApi
  def of(position: JPosition, outcome: JOutcome) =
    JHistoricOutcome(HistoricOutcome(position.asScala, outcome.asScala))

  override def fromJson(jsonString: String): VEither[Problem, JHistoricOutcome] =
    super.fromJson(jsonString)

  protected def jsonEncoder = HistoricOutcome.jsonCodec
  protected def jsonDecoder = HistoricOutcome.jsonCodec
}
