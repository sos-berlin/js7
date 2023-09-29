package js7.data_for_java.order

import io.vavr.control.Either as VEither
import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.base.problem.Problem
import js7.data.order.HistoricOutcome
import js7.data.order.OrderEvent.OrderResumed.{AppendHistoricOutcome, DeleteHistoricOutcome, HistoryOperation, InsertHistoricOutcome, ReplaceHistoricOutcome}
import js7.data_for_java.common.JJsonable
import js7.data_for_java.workflow.position.JPosition

final case class JHistoryOperation(asScala: HistoryOperation)
extends JJsonable[JHistoryOperation]:
  type AsScala = HistoryOperation
  protected def companion = JHistoryOperation

object JHistoryOperation extends JJsonable.Companion[JHistoryOperation]:
  type AsScala = HistoryOperation

  @javaApi @Nonnull
  def replace(@Nonnull historicOutcome: HistoricOutcome): JHistoryOperation =
    JHistoryOperation(ReplaceHistoricOutcome(historicOutcome))

  @javaApi @Nonnull
  def delete(@Nonnull position: JPosition): JHistoryOperation =
    JHistoryOperation(DeleteHistoricOutcome(position.asScala))

  @javaApi @Nonnull
  def insert(@Nonnull before: JPosition, @Nonnull historicOutcome: HistoricOutcome)
  : JHistoryOperation =
    JHistoryOperation(InsertHistoricOutcome(before.asScala, historicOutcome))

  @javaApi @Nonnull
  def append(@Nonnull historicOutcome: HistoricOutcome): JHistoryOperation =
    JHistoryOperation(AppendHistoricOutcome(historicOutcome))

  @Nonnull
  override def fromJson(@Nonnull jsonString: String): VEither[Problem, JHistoryOperation] =
    super.fromJson(jsonString)

  protected def jsonEncoder = HistoryOperation.jsonCodec
  protected def jsonDecoder = HistoryOperation.jsonCodec
