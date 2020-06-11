package js7.tests.history

import io.circe.{Decoder, Encoder, Json}
import js7.base.time.Timestamp
import js7.base.utils.ScalaUtils.RichJavaClass
import js7.data.order.Outcome.Completed
import js7.data.order.{OrderId, Outcome}
import js7.data.system.StdoutOrStderr
import js7.data.workflow.position.WorkflowPosition
import js7.tests.history.OrderEntry._

/**
  * @author Joacim Zschimmer
  */
final case class OrderEntry(
  orderId: OrderId,
  parent: Option[OrderId] = None,
  keyValues: Map[String, String],
  cause: Cause,
  startWorkflowPosition: Option[WorkflowPosition] = None,
  scheduledFor: Option[Timestamp] = None,
  startedAt: Option[Timestamp] = None,
  finishedAt: Option[Timestamp] = None,
  endWorkflowPosition: Option[WorkflowPosition] = None,
  steps: Seq[OrderStepEntry] = Vector.empty)
{
  def updateLastStep(endedAt: Timestamp, outcome: Outcome, keyValues: Map[String, String]): OrderEntry = {
    val lastStep = steps.last
    copy(steps = steps.take(steps.size - 1) :+
      lastStep.copy(
        endedAt = Some(endedAt),
        returnCode = Some(outcome) collect { case o: Completed => o.returnCode },
        endVariables = Some(keyValues)))
  }

  def addToLog(outErr: StdoutOrStderr, chunk: String): OrderEntry =
    updateLog(prefixLines(outErr, chunk))

  private def prefixLines(outErr: StdoutOrStderr, chunk: String) =
    (chunk split '\n' map (o => s"$outErr: $o") mkString "\n") + "\n"

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
    case object Added extends Cause
    case object Forked extends Cause
  }

  implicit val jsonEncoder: Encoder[Cause] = o => Json.fromString(o.getClass.simpleScalaName)
  implicit val jsonDecoder: Decoder[Cause] = _.as[String] map {
    case "Added" => Cause.Added
    case "Forked" => Cause.Forked
  }
}
