package js7.data_for_java.order

import io.vavr.control.Either as VEither
import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.base.problem.Problem
import js7.data.order.HistoricOutcome
import js7.data_for_java.common.JJsonable
import js7.data_for_java.workflow.position.JPosition

/** HistoricOutcome in a Workflow. */
final case class JHistoricOutcome(asScala: HistoricOutcome)
extends JJsonable[JHistoricOutcome]:

  type AsScala = HistoricOutcome

  protected def companion = JHistoricOutcome


object JHistoricOutcome extends JJsonable.Companion[JHistoricOutcome]:
  type AsScala = HistoricOutcome

  @javaApi @Nonnull
  def of(@Nonnull position: JPosition, @Nonnull outcome: JOutcome): JHistoricOutcome =
    JHistoricOutcome(HistoricOutcome(position.asScala, outcome.asScala))

  @Nonnull
  override def fromJson(@Nonnull jsonString: String): VEither[Problem, JHistoricOutcome] =
    super.fromJson(jsonString)

  protected def jsonEncoder = HistoricOutcome.jsonCodec
  protected def jsonDecoder = HistoricOutcome.jsonCodec
