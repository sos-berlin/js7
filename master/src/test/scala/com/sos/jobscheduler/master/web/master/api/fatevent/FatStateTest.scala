package com.sos.jobscheduler.master.web.master.api.fatevent

import com.sos.jobscheduler.base.web.Uri
import com.sos.jobscheduler.core.crypt.silly.{SillySignatureVerifier, SillySigner}
import com.sos.jobscheduler.core.filebased.{FileBasedSigner, FileBasedVerifier, Repo}
import com.sos.jobscheduler.data.agent.{AgentRef, AgentRefPath}
import com.sos.jobscheduler.data.event.{Event, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.fatevent.OrderFatEvent.{OrderAddedFat, OrderCanceledFat, OrderFailedFat, OrderFinishedFat, OrderForkedFat, OrderJoinedFat, OrderProcessedFat, OrderProcessingStartedFat, OrderStderrWrittenFat, OrderStdoutWrittenFat}
import com.sos.jobscheduler.data.filebased.{RepoEvent, VersionId}
import com.sos.jobscheduler.data.job.{ExecutablePath, ReturnCode}
import com.sos.jobscheduler.data.master.{MasterFileBaseds, MasterId}
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderCanceled, OrderDetachable, OrderFailed, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStderrWritten, OrderStdoutWritten, OrderTransferredToAgent, OrderTransferredToMaster}
import com.sos.jobscheduler.data.order.{OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.instructions.Execute
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.master.web.master.api.fatevent.FatStateTest._
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class FatStateTest extends AnyFreeSpec
{
  private val sign = new FileBasedSigner(new SillySigner, MasterFileBaseds.jsonCodec).sign _
  private val repo = Repo.signatureVerifying(new FileBasedVerifier(new SillySignatureVerifier, MasterFileBaseds.jsonCodec))
  private val eventIds = Iterator.from(1)
  private var fatState = FatState(MasterId("MASTER-ID"), eventIds.next(), repo, Map.empty)
  private var beforeFinished: FatState = null

  "VersionAdded" in {
    check(RepoEvent.VersionAdded(versionId),
      None)
  }

  "RepoAdded AgentRef" in {
    check(RepoEvent.FileBasedAdded(agentRef.path, sign(agentRef)),
      None)
  }

  "RepoAdded Workflow" in {
    check(RepoEvent.FileBasedAdded(workflow.path, sign(workflow)),
      None)
  }

  "OrderAdded" in {
    check(orderId <-: OrderAdded(workflow.id, None),
      Some(orderId <-: OrderAddedFat(workflow.id /: Position(0), None, Map.empty)))
  }

  "OrderAttachable" in {
    check(orderId <-: OrderAttachable(agentRef.path),
      None)
  }

  "OrderTransferredToAgent" in {
    check(orderId <-: OrderTransferredToAgent(agentRef.path),
      None)
  }

  "OrderStarted" in {
    check(orderId <-: OrderStarted,
      None)
  }

  "OrderProcessingStarted" in {
    check(orderId <-: OrderProcessingStarted,
      Some(orderId <-: OrderProcessingStartedFat(workflow.id /: Position(0), agentRef.path, agentRef.uri, None, Map.empty)))
  }

  "OrderStdoutWritten" in {
    check(orderId <-: OrderStdoutWritten("STDOUT"),
      Some(orderId <-: OrderStdoutWrittenFat("STDOUT")))
  }

  "OrderStderrWritten" in {
    check(orderId <-: OrderStderrWritten("STDERR"),
      Some(orderId <-: OrderStderrWrittenFat("STDERR")))
  }

  "OrderProcessed" in {
    check(orderId <-: OrderProcessed(Outcome.Succeeded(ReturnCode(7))),
      Some(orderId <-: OrderProcessedFat(Outcome.Succeeded(ReturnCode(7)), Map.empty)))
  }

  "OrderMoved" in {
    check(orderId <-: OrderMoved(Position(1)),
      None)
  }

  "OrderForked" in {
    check(orderId <-: OrderForked(OrderForked.Child("A", orderId / "A") :: OrderForked.Child("B", orderId / "B") :: Nil),
      Some(orderId <-: OrderForkedFat(workflow.id /: Position(1), OrderForkedFat.Child("A", orderId / "A", Map.empty) :: OrderForkedFat.Child("B", orderId / "B", Map.empty) :: Nil)))
  }

  "OrderDetachable" in {
    check(orderId <-: OrderDetachable,
      None)
  }

  "OrderTransferredToMaster" in {
    check(orderId <-: OrderTransferredToMaster,
      None)
  }

  "OrderJoined" in {
    check(orderId <-: OrderJoined(Outcome.Succeeded(ReturnCode(8))),
      Some(orderId <-: OrderJoinedFat(orderId / "A" :: orderId / "B" :: Nil, Outcome.Succeeded(ReturnCode(8)))))
  }

  "OrderMoved (2)" in {
    check(orderId <-: OrderMoved(Position(2)),
      None)
  }

  "OrderFinished" in {
    beforeFinished = fatState
    check(orderId <-: OrderFinished,
      Some(orderId <-: OrderFinishedFat(workflow.id /: Position(2))))
  }

  "OrderFailed" in {
    fatState = beforeFinished
    val outcome = Outcome.Failed(Some("ERROR"), ReturnCode(1))
    check(orderId <-: OrderFailed(outcome),
      Some(orderId <-: OrderFailedFat(workflow.id /: Position(2), outcome)))
  }

  "OrderCanceled" in {
    fatState = beforeFinished
    check(orderId <-: OrderCanceled,
      Some(orderId <-: OrderCanceledFat(workflow.id /: Position(2))))
  }

  private def check(keyedEvent: KeyedEvent[Event], fatEvent: Option[KeyedEvent[Event]]): Unit = {
    val eventId = eventIds.next()
    val (updatedFatState, fatEvents) = fatState.toFatEvents(Stamped(eventId, keyedEvent))
    assert(fatEvents == fatEvent.map(e =>Stamped(eventId, e)))
    assert(updatedFatState.eventId == eventId)
    fatState = updatedFatState
  }
}

object FatStateTest
{
  private val versionId = VersionId("1")
  private val orderId = OrderId("ORDER")
  private val agentRef = AgentRef(AgentRefPath("/AGENT") ~ versionId, Uri("https://0.0.0.0/"))
  private val workflow = Workflow.of(WorkflowPath("/WORKFLOW") ~ versionId,
    Execute.Anonymous(WorkflowJob(agentRef.path, ExecutablePath("/EXECUTABLE"))))
}
