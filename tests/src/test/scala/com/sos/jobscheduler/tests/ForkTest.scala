package com.sos.jobscheduler.tests

import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.base.circeutils.CirceUtils._
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.problem.Problem
import com.sos.jobscheduler.base.utils.MapDiff
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.event.EventSeq
import com.sos.jobscheduler.data.filebased.VersionId
import com.sos.jobscheduler.data.job.ExecutablePath
import com.sos.jobscheduler.data.order.OrderEvent.{OrderAdded, OrderDetachable, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStdoutWritten, OrderStopped, OrderTransferredToAgent, OrderTransferredToMaster}
import com.sos.jobscheduler.data.order.{FreshOrder, OrderEvent, OrderId, Outcome, Payload}
import com.sos.jobscheduler.data.workflow.instructions.Execute
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting
import com.sos.jobscheduler.data.workflow.test.ForkTestSetting._
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowPath}
import com.sos.jobscheduler.tester.CirceJsonTester.testJson
import com.sos.jobscheduler.tests.DirectoryProvider.{StdoutOutput, script}
import com.sos.jobscheduler.tests.ForkTest._
import com.typesafe.config.ConfigFactory
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.concurrent.duration._
import scala.language.higherKinds

final class ForkTest extends FreeSpec with DirectoryProvider.ForScalaTest
{
  protected val agentPaths = AAgentPath :: BAgentPath :: Nil
  override protected val masterConfig = ConfigFactory.parseString(
    s"""jobscheduler.TEST-ONLY.suppress-order-id-check-for = "DUPLICATE/🥕" """)

  override def beforeAll() = {
    directoryProvider.master.writeJson(TestWorkflow.withoutVersion)
    directoryProvider.master.writeJson(Workflow(
      DuplicateWorkflowPath % VersionId.Anonymous,
      Vector(
        Execute(WorkflowJob(AAgentPath, ExecutablePath("/SLOW"))))))
    directoryProvider.agents(0).writeExecutable(ExecutablePath("/SLOW"), script(60.s))
    for (a ← directoryProvider.agents) a.writeExecutable(TestExecutablePath, script(100.ms))
    super.beforeAll()
  }

  "test" in {
    master.addOrderBlocking(TestOrder)
    master.eventWatch.await[OrderFinished](_.key == TestOrder.id)
    master.eventWatch.all[OrderEvent] match {
      case EventSeq.NonEmpty(stampeds) ⇒
        val keyedEvents = stampeds.map(_.value).toVector
        for (orderId ← Array(TestOrder.id, XOrderId, YOrderId)) {  // But ordering if each order is determined
          assert(keyedEvents.filter(_.key == orderId) == ExpectedEvents.filter(_.key == orderId))
        }
        assert(keyedEvents.toSet == ExpectedEvents.toSet)  // XOrderId and YOrderId run in parallel and ordering is not determined
      case o ⇒
        fail(s"Unexpected EventSeq received: $o")
    }
  }

  "Existing child OrderId" in {
    val myOrderId = TestOrder.copy(id = OrderId("DUPLICATE"))
    master.addOrderBlocking(FreshOrder(OrderId("DUPLICATE/🥕"), DuplicateWorkflowPath))  // Invalid syntax is allowed for this OrderId
    master.addOrderBlocking(myOrderId)
    assert(master.eventWatch.await[OrderStopped](_.key == myOrderId.id).head.value.event ==
      OrderStopped(Outcome.Disrupted(Problem("Forked OrderIds duplicate existing Order(Order:DUPLICATE/🥕,Workflow:/DUPLICATE (initial)/#0,InProcess,Some(Agent(Agent:/AGENT-A (initial))),None,Payload())"))))

    // Kill SLOW job
    agents(0).executeCommand(AgentCommand.Terminate(sigkillProcessesAfter = Some(0.seconds))).await(99.s).orThrow
    agents(0).terminated await 99.s
  }

  "JSON" in {
    testJson(ForkTestSetting.TestWorkflow, json"""{
      "id": {
         "path": "/WORKFLOW",
         "versionId": "(initial)"
       },
      "source": "$TestWorkflowSource",
      "instructions": [
        { "TYPE": "Execute.Anonymous", "job": { "executablePath": "/executable", "taskLimit": 1, "agentPath": "/AGENT-A" }},
        {
          "TYPE": "ForkJoin",
          "branches": [
            {
              "id": "🥕",
              "workflow": {
                "instructions": [
                  { "TYPE": "Execute.Anonymous", "job": { "executablePath": "/executable", "taskLimit": 1, "agentPath": "/AGENT-A" }},
                  { "TYPE": "Execute.Named", "name": "A" }
                ]
              }
            }, {
              "id": "🍋",
              "workflow": {
                "instructions": [
                  { "TYPE": "Execute.Anonymous", "job": { "executablePath": "/executable", "taskLimit": 1, "agentPath": "/AGENT-A" }},
                  { "TYPE": "Execute.Named", "name": "B" }
                ]
              }
            }
          ]
        },
        { "TYPE": "Execute.Anonymous", "job": { "executablePath": "/executable", "taskLimit": 1, "agentPath": "/AGENT-A" }},
        {
          "TYPE": "ForkJoin",
          "branches": [
            {
              "id": "🥕",
              "workflow": {
                "instructions": [
                  { "TYPE": "Execute.Anonymous", "job": { "executablePath": "/executable", "taskLimit": 1, "agentPath": "/AGENT-A" }},
                  { "TYPE": "Execute.Named", "name": "A" }
                ]
              }
            }, {
              "id": "🍋",
              "workflow": {
                "instructions": [
                  { "TYPE": "Execute.Anonymous", "job": { "executablePath": "/executable", "taskLimit": 1, "agentPath": "/AGENT-A" }},
                  { "TYPE": "Execute.Named", "name": "A" }
                ]
              }
            }
          ]
        },
        { "TYPE": "Execute.Anonymous", "job": { "executablePath": "/executable", "taskLimit": 1, "agentPath": "/AGENT-A" }},
        {
          "TYPE": "ForkJoin",
          "branches": [
            {
              "id": "🥕",
              "workflow": {
                "instructions": [
                  { "TYPE": "Execute.Anonymous", "job": { "executablePath": "/executable", "taskLimit": 1, "agentPath": "/AGENT-A" }},
                  { "TYPE": "Execute.Named", "name": "A" }
                ]
              }
            }, {
              "id": "🍋",
              "workflow": {
                "instructions": [
                  { "TYPE": "Execute.Anonymous", "job": { "executablePath": "/executable", "taskLimit": 1, "agentPath": "/AGENT-B" }},
                  { "TYPE": "Execute.Named", "name": "B" }
                ]
              }
            }
          ]
        },
        { "TYPE": "Execute.Named", "name": "A" }
      ],
      "jobs": {
        "A": { "agentPath": "/AGENT-A", "executablePath": "/executable", "taskLimit": 1 },
        "B": { "agentPath": "/AGENT-B", "executablePath": "/executable", "taskLimit": 1 }
      }
    }""")
  }
}

object ForkTest {
  private val DuplicateWorkflowPath = WorkflowPath("/DUPLICATE")
  private val TestOrder = FreshOrder(OrderId("🔺"), TestWorkflow.id.path, payload = Payload(Map("VARIABLE" → "VALUE")))
  private val XOrderId = OrderId(s"🔺/🥕")
  private val YOrderId = OrderId(s"🔺/🍋")
  private val ExpectedEvents = Vector(
    TestOrder.id <-: OrderAdded(TestWorkflow.id, None, Payload(Map("VARIABLE" → "VALUE"))),
    TestOrder.id <-: OrderTransferredToAgent(AAgentPath % "(initial)"),
    TestOrder.id <-: OrderProcessingStarted,
    TestOrder.id <-: OrderStdoutWritten(StdoutOutput),
    TestOrder.id <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),
    TestOrder.id <-: OrderMoved(Position(1)),

    TestOrder.id <-: OrderForked(Vector(
      OrderForked.Child("🥕", XOrderId, MapDiff.empty),                        OrderForked.Child("🍋", YOrderId, MapDiff.empty))),
    TestOrder.id <-: OrderDetachable,
    TestOrder.id <-: OrderTransferredToMaster,
      XOrderId <-: OrderProcessingStarted,                                     YOrderId <-: OrderProcessingStarted,
      XOrderId <-: OrderStdoutWritten(StdoutOutput),                           YOrderId <-: OrderStdoutWritten(StdoutOutput),
      XOrderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),           YOrderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),
      XOrderId <-: OrderMoved(Position(1, "🥕", 1)),                           YOrderId <-: OrderMoved(Position(1, "🍋", 1)),
    YOrderId <-: OrderDetachable,
                                                                               YOrderId <-: OrderTransferredToMaster,
                                                                               YOrderId <-: OrderTransferredToAgent(BAgentPath % "(initial)"),
      XOrderId <-: OrderProcessingStarted,                                     YOrderId <-: OrderProcessingStarted,
      XOrderId <-: OrderStdoutWritten(StdoutOutput),                           YOrderId <-: OrderStdoutWritten(StdoutOutput),
      XOrderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),           YOrderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),
      XOrderId <-: OrderMoved(Position(1, "🥕", 2)),                           YOrderId <-: OrderMoved(Position(1, "🍋", 2)),
      XOrderId <-: OrderDetachable,                                            YOrderId <-: OrderDetachable,
      XOrderId <-: OrderTransferredToMaster,                                   YOrderId <-: OrderTransferredToMaster,
    TestOrder.id <-: OrderJoined(MapDiff.empty, Outcome.succeeded),
    TestOrder.id <-: OrderMoved(Position(2)),

    TestOrder.id <-: OrderTransferredToAgent(AAgentPath % "(initial)"),
    TestOrder.id <-: OrderProcessingStarted,
    TestOrder.id <-: OrderStdoutWritten(StdoutOutput),
    TestOrder.id <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),
    TestOrder.id <-: OrderMoved(Position(3)),
    TestOrder.id <-: OrderForked(Vector(
      OrderForked.Child("🥕", XOrderId, MapDiff.empty),                        OrderForked.Child("🍋", YOrderId, MapDiff.empty))),
    TestOrder.id <-: OrderDetachable,
    TestOrder.id <-: OrderTransferredToMaster,
      XOrderId <-: OrderProcessingStarted,                                     YOrderId <-: OrderProcessingStarted,
      XOrderId <-: OrderStdoutWritten(StdoutOutput),                           YOrderId <-: OrderStdoutWritten(StdoutOutput),
      XOrderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),           YOrderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),
      XOrderId <-: OrderMoved(Position(3, "🥕", 1)),                           YOrderId <-: OrderMoved(Position(3, "🍋", 1)),
      XOrderId <-: OrderProcessingStarted,                                     YOrderId <-: OrderProcessingStarted,
      XOrderId <-: OrderStdoutWritten(StdoutOutput),                           YOrderId <-: OrderStdoutWritten(StdoutOutput),
      XOrderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),           YOrderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),
      XOrderId <-: OrderMoved(Position(3, "🥕", 2)),                           YOrderId <-: OrderMoved(Position(3, "🍋", 2)),
      XOrderId <-: OrderDetachable,                                            YOrderId <-: OrderDetachable,
      XOrderId <-: OrderTransferredToMaster,                                   YOrderId <-: OrderTransferredToMaster,
    TestOrder.id <-: OrderJoined(MapDiff.empty, Outcome.succeeded),
    TestOrder.id <-: OrderMoved(Position(4)),

    TestOrder.id <-: OrderTransferredToAgent(AAgentPath % "(initial)"),
    TestOrder.id <-: OrderProcessingStarted,
    TestOrder.id <-: OrderStdoutWritten(StdoutOutput),
    TestOrder.id <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),
    TestOrder.id <-: OrderMoved(Position(5)),
  //TestOrder.id <-: OrderDetachable,
  //TestOrder.id <-: OrderTransferredToMaster,

    TestOrder.id <-: OrderForked(Vector(
      OrderForked.Child("🥕", XOrderId, MapDiff.empty),                        OrderForked.Child("🍋", YOrderId, MapDiff.empty))),
    TestOrder.id <-: OrderDetachable,
    TestOrder.id <-: OrderTransferredToMaster,
                                                                               YOrderId <-: OrderDetachable,
                                                                               YOrderId <-: OrderTransferredToMaster,
                                                                               YOrderId <-: OrderTransferredToAgent(BAgentPath % "(initial)"),
      XOrderId <-: OrderProcessingStarted,                                     YOrderId <-: OrderProcessingStarted,
      XOrderId <-: OrderStdoutWritten(StdoutOutput),                           YOrderId <-: OrderStdoutWritten(StdoutOutput),
      XOrderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),           YOrderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),
      XOrderId <-: OrderMoved(Position(5, "🥕", 1)),                           YOrderId <-: OrderMoved(Position(5, "🍋", 1)),
      XOrderId <-: OrderProcessingStarted,                                     YOrderId <-: OrderProcessingStarted,
      XOrderId <-: OrderStdoutWritten(StdoutOutput),                           YOrderId <-: OrderStdoutWritten(StdoutOutput),
      XOrderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),           YOrderId <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),
      XOrderId <-: OrderMoved(Position(5, "🥕", 2)),                           YOrderId <-: OrderMoved(Position(5, "🍋", 2)),
      XOrderId <-: OrderDetachable,                                            YOrderId <-: OrderDetachable,
      XOrderId <-: OrderTransferredToMaster,                                   YOrderId <-: OrderTransferredToMaster,
    TestOrder.id <-: OrderJoined(MapDiff.empty, Outcome.succeeded),
    TestOrder.id <-: OrderMoved(Position(6)),

    TestOrder.id <-: OrderTransferredToAgent(AAgentPath % "(initial)"),
    TestOrder.id <-: OrderProcessingStarted,
    TestOrder.id <-: OrderStdoutWritten(StdoutOutput),
    TestOrder.id <-: OrderProcessed(MapDiff.empty, Outcome.succeeded),
    TestOrder.id <-: OrderMoved(Position(7)),
    TestOrder.id <-: OrderDetachable,
    TestOrder.id <-: OrderTransferredToMaster,
    TestOrder.id <-: OrderFinished)
}
