package com.sos.jobscheduler.tests.history

import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Closers.withCloser
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXmls.implicits.RichXmlPath
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.{Event, EventId, EventSeq, TearableEventSeq}
import com.sos.jobscheduler.data.filebased.SourceType
import com.sos.jobscheduler.data.job.{JobPath, ReturnCode}
import com.sos.jobscheduler.data.order.OrderEvent.OrderFinished
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId, Payload}
import com.sos.jobscheduler.data.workflow.{Position, WorkflowPath}
import com.sos.jobscheduler.master.KeyedEventJsonCodecs.MasterKeyedEventJsonCodec
import com.sos.jobscheduler.master.client.AkkaHttpMasterApi
import com.sos.jobscheduler.master.tests.TestEventCollector
import com.sos.jobscheduler.tests.DirectoryProvider
import com.sos.jobscheduler.tests.DirectoryProvider.jobXml
import com.sos.jobscheduler.tests.history.HistoryEventsTest._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.collection.immutable.IndexedSeq
import scala.concurrent.duration._
import scala.language.{higherKinds, implicitConversions}

/**
  * @author Joacim Zschimmer
  */
final class HistoryEventsTest extends FreeSpec {

  "test" in {
    autoClosing(new DirectoryProvider(List(AAgentPath, BAgentPath))) { provider ⇒
      withCloser { implicit closer ⇒
        provider.master.writeTxt(TestWorkflowId.path, TestWorkflowNotation)
        for (a ← provider.agents) a.file(TestJobPath, SourceType.Xml).xml = jobXml(0.s)

        provider.runAgents() { runningAgents ⇒
          val eventCollector = new TestEventCollector
          provider.runMaster(Some(eventCollector)) { master ⇒
            autoClosing(new AkkaHttpMasterApi(master.localUri)) { api ⇒
              val TearableEventSeq.Torn(oldestEventId) = api.events[Event](after = EventId.BeforeFirst, 1.second) await 99.s

              master.addOrderBlocking(TestOrder)
              eventCollector.await[OrderFinished](_.key == TestOrder.id)

              val EventSeq.NonEmpty(stampeds) = api.events[Event](after = oldestEventId, 1.second) await 99.s

              val history = new TestHistory
              stampeds foreach history.handleStampedKeyedEvent

              assert(history.orderEntries.map(normalizeTimestampsInEntry) ==
                expectedOrderEntries(runningAgents map (_.localUri.toString)))
            }
          }
        }
      }
    }
  }
}

object HistoryEventsTest {
  private val AAgentPath = AgentPath("/AGENT-A")
  private val BAgentPath = AgentPath("/AGENT-B")
  private val TestJobPath = JobPath("/JOB")
  private val TestWorkflowId = WorkflowPath("/WORKFLOW") % "(initial)"
  private val TestWorkflowNotation = """
     |job "JOB" on "AGENT-A";
     |fork(
     |  "🥕" {
     |    job "JOB" on "AGENT-A";
     |    job "JOB" on "AGENT-A";
     |  },
     |  "🍋" {
     |    job "JOB" on "AGENT-A";
     |    job "JOB" on "AGENT-B";
     |  });
     |job "JOB" on "AGENT-A";
     """.stripMargin.trim

  private val TestOrder = FreshOrder(OrderId("🔺"), TestWorkflowId.path, payload = Payload(Map("VARIABLE" → "VALUE")))
  private val TestTimestamp = Timestamp.ofEpochMilli(0)


  private def expectedOrderEntries(agentUris: IndexedSeq[String]) = {
    implicit def toSome[A](a: A): Option[A] = Some(a)
    Vector(
      OrderEntry(
        TestOrder.id, None, OrderEntry.Cause.UNKNOWN, TestWorkflowId /: Position(0), TestTimestamp, None, endedAt = TestTimestamp, TestWorkflowId /: Position(3),
        Vector(
          OrderStepEntry(TestOrder.id, TestWorkflowId /: Position(0), agentUri = agentUris(0), TestJobPath, Map("VARIABLE" → "VALUE"), TestTimestamp, None, ReturnCode(0), Map("VARIABLE" → "VALUE"), "stdout: TEST ☘"),
          OrderStepEntry(OrderId("🔺"), TestWorkflowId /: Position(2), agentUris(0), TestJobPath, Map("VARIABLE" → "VALUE"), TestTimestamp, None, ReturnCode(0), Map("VARIABLE" → "VALUE"), "stdout: TEST ☘"))),
      OrderEntry(OrderId("🔺/🥕"), None, OrderEntry.Cause.Forked, TestWorkflowId /: Position(1, "🥕", 0), TestTimestamp, None, None, None,
        steps = Vector(
          OrderStepEntry(OrderId("🔺/🥕"), TestWorkflowId /: Position(1, "🥕", 0), agentUris(0), TestJobPath, Map("VARIABLE" → "VALUE"), TestTimestamp, None, ReturnCode(0), Map("VARIABLE" → "VALUE"), "stdout: TEST ☘"),
          OrderStepEntry(OrderId("🔺/🥕"), TestWorkflowId /: Position(1, "🥕", 1), agentUris(0), TestJobPath, Map("VARIABLE" → "VALUE"), TestTimestamp, None, ReturnCode(0), Map("VARIABLE" → "VALUE"), "stdout: TEST ☘"))),
      OrderEntry(OrderId("🔺/🍋"), None, OrderEntry.Cause.Forked, TestWorkflowId /: Position(1, "🍋", 0), TestTimestamp, None, None, None,
        steps = Vector(
          OrderStepEntry(OrderId("🔺/🍋"), TestWorkflowId /: Position(1, "🍋", 0), agentUris(0), TestJobPath, Map("VARIABLE" → "VALUE"), TestTimestamp, None, ReturnCode(0), Map("VARIABLE" → "VALUE"), "stdout: TEST ☘"),
          OrderStepEntry(OrderId("🔺/🍋"), TestWorkflowId /: Position(1, "🍋", 1), agentUris(1), TestJobPath, Map("VARIABLE" → "VALUE"), TestTimestamp, None, ReturnCode(0), Map("VARIABLE" → "VALUE"), "stdout: TEST ☘"))))
    }

  private def normalizeTimestampsInEntry(entry: OrderEntry): OrderEntry =
    entry.copy(
      startedAt = entry.startedAt map (_ ⇒ TestTimestamp),
      scheduledAt = entry.scheduledAt map (_ ⇒ TestTimestamp),
      endedAt = entry.endedAt map (_ ⇒ TestTimestamp),
      steps = entry.steps map normalizeTimestampsInStep)

  private def normalizeTimestampsInStep(step: OrderStepEntry): OrderStepEntry =
    step.copy(
      startedAt = TestTimestamp,
      endedAt = step.endedAt map (_ ⇒ TestTimestamp))
}
