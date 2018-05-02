package com.sos.jobscheduler.tests.history

import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.data.order.Outcome.Undisrupted
import com.sos.jobscheduler.data.order.{OrderFatEvent, OrderId, Outcome}
import com.sos.jobscheduler.data.system.StdoutOrStderr
import com.sos.jobscheduler.data.workflow.WorkflowPosition
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
final case class OrderEntry(
  orderId: OrderId,
  parent: Option[OrderId] = None,
  cause: OrderFatEvent.OrderAddedFat.Cause,
  startWorkflowPosition: Option[WorkflowPosition] = None,
  scheduledAt: Option[Timestamp] = None,
  startedAt: Option[Timestamp] = None,
  endedAt: Option[Timestamp] = None,
  endWorkflowPosition: Option[WorkflowPosition] = None,
  steps: Seq[OrderStepEntry] = Vector.empty)
{
  def updateLastStep(endedAt: Timestamp, outcome: Outcome, variables: Map[String, String]): OrderEntry = {
    val lastStep = steps.last
    copy(steps = steps.take(steps.size - 1) :+
      lastStep.copy(
        endedAt = Some(endedAt),
        returnCode = Some(outcome) collect { case o: Undisrupted ⇒ o.returnCode },
        endVariables = Some(variables)))
  }

  def addToLog(outErr: StdoutOrStderr, chunk: String): OrderEntry =
    updateLog(prefixLines(outErr, chunk))

  private def prefixLines(outErr: StdoutOrStderr, chunk: String) =
    chunk split "\n" map (o ⇒ s"$outErr: $o") mkString "\n"

  def updateLog(chunk: String): OrderEntry = {
    val lastStep = steps.last
    copy(
      steps = steps.take(steps.size - 1) :+
        lastStep.copy(
          log = Some(lastStep.log.getOrElse("") + chunk)))
  }
}
