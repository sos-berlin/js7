package com.sos.jobscheduler.tests.history

import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.data.system.StdoutOrStderr
import com.sos.jobscheduler.data.workflow.WorkflowPosition
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
final case class OrderEntry(
  orderId: OrderId,
  parent: Option[OrderId] = None,
  cause: OrderEntry.Cause,
  startWorkflowPosition: Option[WorkflowPosition] = None,
  startedAt: Option[Timestamp] = None,
  scheduledAt: Option[Timestamp] = None,
  endedAt: Option[Timestamp] = None,
  endWorkflowPosition: Option[WorkflowPosition] = None,
  steps: Seq[OrderStepEntry])
{
  def updateLastStep(returnCode: Option[ReturnCode], endVariables: Map[String, String]) = {
    val lastStep = steps.last
    copy(steps = steps.take(steps.size - 1) :+
      lastStep.copy(
        returnCode = returnCode,
        endVariables = Some(endVariables)))
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
object OrderEntry {
  sealed trait Cause
  object Cause {
    case object UNKNOWN extends Cause
    case object Forked extends Cause
  }
}
