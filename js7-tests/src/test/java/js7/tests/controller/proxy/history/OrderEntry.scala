package js7.tests.controller.proxy.history

import java.time.Instant
import java.util.Optional
import js7.data.order.Outcome.Completed
import js7.data.order.{OrderId, Outcome}
import js7.data.system.StdoutOrStderr
import js7.proxy.javaapi.data.workflow.position.JWorkflowPosition
import js7.tests.controller.proxy.history.OrderEntry._
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._

final case class OrderEntry(
  orderId: OrderId,
  parent: Optional[OrderId] = Optional.empty,
  keyValues: java.util.Map[String, String],
  cause: Cause,
  startWorkflowPosition: Optional[JWorkflowPosition] = Optional.empty,
  scheduledFor: Optional[Instant] = Optional.empty,
  startedAt: Optional[Instant] = Optional.empty,
  terminatedAt: Optional[Instant] = Optional.empty,
  endWorkflowPosition: Optional[JWorkflowPosition] = Optional.empty,
  steps: java.util.List[OrderStepEntry] = Vector.empty.asJava)
{
  def updateLastStep(endedAt: Instant, outcome: Outcome, keyValues: java.util.Map[String, String]): OrderEntry = {
    val lastStep = steps.asScala.last
    copy(steps = (steps.asScala.take(steps.size - 1) :+
      lastStep.copy(
        endedAt = Optional.of(endedAt),
        returnCode = Some(outcome).collect { case o: Completed => o.returnCode }.toJava,
        endVariables = Optional.of(keyValues))).asJava)
  }

  def addToLog(outErr: StdoutOrStderr, chunk: String): OrderEntry =
    updateLog(prefixLines(outErr, chunk))

  private def prefixLines(outErr: StdoutOrStderr, chunk: String) =
    (chunk split '\n' map (o => s"$outErr: $o") mkString "\n") + "\n"

  def updateLog(chunk: String): OrderEntry = {
    val lastStep = steps.asScala.last
    copy(steps =
      ( steps.asScala.take(steps.size - 1) :+
        lastStep.copy(
          log = Optional.of(lastStep.log.toScala.getOrElse("") + chunk))
      ).asJava)
  }
}

object OrderEntry
{
  sealed trait Cause
  object Cause {
    case object Added extends Cause
    case object Forked extends Cause
  }
}
