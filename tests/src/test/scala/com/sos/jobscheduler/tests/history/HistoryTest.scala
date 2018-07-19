package com.sos.jobscheduler.tests.history

import com.sos.jobscheduler.base.auth.{UserAndPassword, UserId}
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Closers.withCloser
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits.RichPath
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.core.event.journal.JournalFiles
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.{EventId, EventRequest, EventSeq}
import com.sos.jobscheduler.data.filebased.SourceType
import com.sos.jobscheduler.data.job.{JobPath, ReturnCode}
import com.sos.jobscheduler.data.order.OrderEvent.OrderFinished
import com.sos.jobscheduler.data.order.OrderFatEvent.OrderFinishedFat
import com.sos.jobscheduler.data.order.{FreshOrder, OrderFatEvent, OrderId, Payload}
import com.sos.jobscheduler.data.workflow.{Position, WorkflowPath}
import com.sos.jobscheduler.master.client.AkkaHttpMasterApi
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.tests.DirectoryProvider
import com.sos.jobscheduler.tests.DirectoryProvider.{StdoutOutput, jobJson}
import com.sos.jobscheduler.tests.history.HistoryTest._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.collection.immutable.IndexedSeq
import scala.concurrent.duration._
import scala.language.{higherKinds, implicitConversions}

/**
  * @author Joacim Zschimmer
  */
final class HistoryTest extends FreeSpec
{
  "test" in {
    autoClosing(new DirectoryProvider(List(AAgentPath, BAgentPath))) { provider ⇒
      withCloser { implicit closer ⇒
        (provider.master.config / "private/private.conf").append(
          """jobscheduler.auth.users.TEST-USER = "plain:TEST-PASSWORD"
            |""".stripMargin )
        provider.master.writeTxt(TestWorkflowId.path, TestWorkflowNotation)
        for (a ← provider.agents) a.file(TestJobPath, SourceType.Json).contentString = jobJson(0.s)

        def listJournalFiles = JournalFiles.listJournalFiles(provider.master.data / "state" / "master").map(_.file.getFileName.toString)

        provider.runAgents() { runningAgents ⇒
          provider.runMaster() { master ⇒
            autoClosing(new AkkaHttpMasterApi(master.localUri)) { masterApi ⇒
              masterApi.login(Some(UserAndPassword(UserId("TEST-USER"), SecretString("TEST-PASSWORD")))) await 99.s
              master.addOrderBlocking(TestOrder)
              master.eventReader.await[OrderFinished](_.key == TestOrder.id)
              assert(listJournalFiles == Vector("master--0.journal"))
            }
          }
          provider.runMaster() { master ⇒
            autoClosing(new AkkaHttpMasterApi(master.localUri)) { masterApi ⇒
              masterApi.login(Some(UserAndPassword(UserId("TEST-USER"), SecretString("TEST-PASSWORD")))) await 99.s
              val history = new InMemoryHistory
              var lastEventId = EventId.BeforeFirst
              var finished = false
              var rounds = 0
              while (!finished) {
                rounds += 1
                val request = EventRequest.singleClass[OrderFatEvent](after = lastEventId, timeout = 99.seconds, limit = 2)
                val EventSeq.NonEmpty(stampeds) = masterApi.fatEvents(request) await 99.s
                val chunk = stampeds take 2
                chunk foreach history.handleHistoryEvent
                lastEventId = chunk.last.eventId
                finished = chunk.last.value.event.isInstanceOf[OrderFinishedFat]
              }
              assert(rounds > 2)
              assert(history.orderEntries.map(normalizeTimestampsInEntry) ==
                expectedOrderEntries(runningAgents map (_.localUri.toString)))

              assert(listJournalFiles.length == 2 && listJournalFiles.contains("master--0.journal"))

              masterApi.executeCommand(MasterCommand.KeepEvents(lastEventId -1)) await 99.s
              assert(listJournalFiles.length == 2 && listJournalFiles.contains("master--0.journal"))  // Nothing deleted

              masterApi.executeCommand(MasterCommand.KeepEvents(lastEventId)) await 99.s
              assert(listJournalFiles.length == 1 && !listJournalFiles.contains("master--0.journal"))  // First file deleted
            }
          }
        }
      }
    }
  }
}

object HistoryTest
{
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
        TestOrder.id, None, OrderEntry.Cause.Added, TestWorkflowId /: Position(0), None, TestTimestamp, endedAt = TestTimestamp, TestWorkflowId /: Position(3),
        Vector(
          OrderStepEntry(TestOrder.id, TestWorkflowId /: Position(0), agentUri = agentUris(0), TestJobPath, Map("VARIABLE" → "VALUE"), TestTimestamp, TestTimestamp, ReturnCode(0), Map("VARIABLE" → "VALUE"), s"stdout: $StdoutOutput"),
          OrderStepEntry(OrderId("🔺"), TestWorkflowId /: Position(2), agentUris(0), TestJobPath, Map("VARIABLE" → "VALUE"), TestTimestamp, TestTimestamp, ReturnCode(0), Map("VARIABLE" → "VALUE"), s"stdout: $StdoutOutput"))),
      OrderEntry(OrderId("🔺/🥕"), OrderId("🔺"), OrderEntry.Cause.Forked, TestWorkflowId /: Position(1, "🥕", 0), None, TestTimestamp, None, None,
        steps = Vector(
          OrderStepEntry(OrderId("🔺/🥕"), TestWorkflowId /: Position(1, "🥕", 0), agentUris(0), TestJobPath, Map("VARIABLE" → "VALUE"), TestTimestamp, TestTimestamp, ReturnCode(0), Map("VARIABLE" → "VALUE"), s"stdout: $StdoutOutput"),
          OrderStepEntry(OrderId("🔺/🥕"), TestWorkflowId /: Position(1, "🥕", 1), agentUris(0), TestJobPath, Map("VARIABLE" → "VALUE"), TestTimestamp, TestTimestamp, ReturnCode(0), Map("VARIABLE" → "VALUE"), s"stdout: $StdoutOutput"))),
      OrderEntry(OrderId("🔺/🍋"), OrderId("🔺"), OrderEntry.Cause.Forked, TestWorkflowId /: Position(1, "🍋", 0), None, TestTimestamp, None, None,
        steps = Vector(
          OrderStepEntry(OrderId("🔺/🍋"), TestWorkflowId /: Position(1, "🍋", 0), agentUris(0), TestJobPath, Map("VARIABLE" → "VALUE"), TestTimestamp, TestTimestamp, ReturnCode(0), Map("VARIABLE" → "VALUE"), s"stdout: $StdoutOutput"),
          OrderStepEntry(OrderId("🔺/🍋"), TestWorkflowId /: Position(1, "🍋", 1), agentUris(1), TestJobPath, Map("VARIABLE" → "VALUE"), TestTimestamp, TestTimestamp, ReturnCode(0), Map("VARIABLE" → "VALUE"), s"stdout: $StdoutOutput"))))
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
