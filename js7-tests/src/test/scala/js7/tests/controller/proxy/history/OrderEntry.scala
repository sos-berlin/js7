package js7.tests.controller.proxy.history

import java.time.Instant
import java.util.Optional
import js7.base.io.process.StdoutOrStderr
import js7.data.order.{OrderId, Outcome}
import js7.data.value.Value
import js7.data_for_java.workflow.position.JWorkflowPosition
import js7.tests.controller.proxy.history.OrderEntry.*
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

final case class OrderEntry(
  orderId: OrderId,
  parent: Optional[OrderId] = Optional.empty,
  namedValues: java.util.Map[String, Value],
  cause: Cause,
  startWorkflowPosition: Optional[JWorkflowPosition] = Optional.empty,
  scheduledFor: Optional[Instant] = Optional.empty,
  startedAt: Optional[Instant] = Optional.empty,
  terminatedAt: Optional[Instant] = Optional.empty,
  endWorkflowPosition: Optional[JWorkflowPosition] = Optional.empty,
  steps: java.util.List[OrderStepEntry] = Vector.empty.asJava):

  def updateLastStep(endedAt: Instant, outcome: Outcome, namedValues: java.util.Map[String, Value]): OrderEntry =
    val lastStep = steps.asScala.last
    copy(steps = (steps.asScala.take(steps.size - 1) :+
      lastStep.copy(
        endedAt = Optional.of(endedAt),
        endVariables = Optional.of(namedValues))).asJava)

  def addToLog(outErr: StdoutOrStderr, chunk: String): OrderEntry =
    updateLog(prefixLines(outErr, chunk))

  private def prefixLines(outErr: StdoutOrStderr, chunk: String) =
    (chunk.split('\n').map(o => s"$outErr: $o") mkString "\n") + "\n"

  def updateLog(chunk: String): OrderEntry =
    val lastStep = steps.asScala.last
    copy(steps =
      ( steps.asScala.take(steps.size - 1) :+
        lastStep.copy(
          log = Optional.of(lastStep.log.toScala.getOrElse("") + chunk))
      ).asJava)


object OrderEntry:
  sealed trait Cause
  object Cause:
    case object Added extends Cause
    case object Forked extends Cause
