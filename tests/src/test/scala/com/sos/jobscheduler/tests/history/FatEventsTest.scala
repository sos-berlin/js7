package com.sos.jobscheduler.tests.history

import com.sos.jobscheduler.base.auth.{UserAndPassword, UserId}
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.common.process.Processes.{ShellFileExtension ⇒ sh}
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits.RichPath
import com.sos.jobscheduler.common.scalautil.MonixUtils.ops._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.core.event.journal.files.JournalFiles
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.event.KeyedEvent.NoKey
import com.sos.jobscheduler.data.event.{EventId, EventRequest, EventSeq, KeyedEvent, Stamped, TearableEventSeq}
import com.sos.jobscheduler.data.fatevent.AgentFatEvent.AgentReadyFat
import com.sos.jobscheduler.data.fatevent.FatEvent
import com.sos.jobscheduler.data.fatevent.MasterFatEvent.MasterReadyFat
import com.sos.jobscheduler.data.fatevent.OrderFatEvent.{OrderAddedFat, OrderFinishedFat, OrderForkedFat, OrderJoinedFat, OrderProcessedFat, OrderProcessingStartedFat, OrderStdoutWrittenFat}
import com.sos.jobscheduler.data.job.{ExecutablePath, ReturnCode}
import com.sos.jobscheduler.data.master.MasterId
import com.sos.jobscheduler.data.order.OrderEvent.OrderFinished
import com.sos.jobscheduler.data.order.Outcome.Succeeded
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId, Payload}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.position.{BranchId, Position}
import com.sos.jobscheduler.master.client.AkkaHttpMasterApi
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.tests.DirectoryProvider
import com.sos.jobscheduler.tests.DirectoryProvider.StdoutOutput
import com.sos.jobscheduler.tests.history.FatEventsTest._
import java.time.ZoneId
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.collection.immutable.IndexedSeq
import scala.collection.mutable
import scala.concurrent.duration._
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
final class FatEventsTest extends FreeSpec
{
  "test" in {
    autoClosing(new DirectoryProvider(List(AAgentPath, BAgentPath))) { provider ⇒
      (provider.master.config / "private/private.conf").append("""
        |jobscheduler.auth.users.TEST-USER = "plain:TEST-PASSWORD"
        |""".stripMargin )
      provider.master.writeTxt(TestWorkflowId.path, TestWorkflowNotation)
      for (a ← provider.agents) a.writeExecutable(TestExecutablePath, DirectoryProvider.script(0.s))

      def listJournalFiles = JournalFiles.listJournalFiles(provider.master.data / "state" / "master").map(_.file.getFileName.toString)

      provider.runAgents() { runningAgents ⇒
        provider.runMaster() { master ⇒
          autoClosing(new AkkaHttpMasterApi(master.localUri)) { masterApi ⇒
            masterApi.login(Some(UserAndPassword(UserId("TEST-USER"), SecretString("TEST-PASSWORD")))) await 99.s
            master.addOrderBlocking(TestOrder)
            master.eventWatch.await[OrderFinished](_.key == TestOrder.id)
            assert(listJournalFiles == Vector("master--0.journal"))
          }
        }
        var keepEventsEventId = EventId.BeforeFirst
        val fatEvents = mutable.Buffer[KeyedEvent[FatEvent]]()
        provider.runMaster() { master ⇒
          autoClosing(new AkkaHttpMasterApi(master.localUri)) { masterApi ⇒
            masterApi.login(Some(UserAndPassword(UserId("TEST-USER"), SecretString("TEST-PASSWORD")))) await 99.s
            val history = new InMemoryHistory
            var lastEventId = EventId.BeforeFirst
            var finished = false
            var finishedEventId = -1L
            var rounds = 0
            while (!finished) {
              rounds += 1
              val request = EventRequest.singleClass[FatEvent](after = lastEventId, timeout = 99.seconds, limit = 2)
              val EventSeq.NonEmpty(stampeds) = masterApi.fatEvents(request) await 99.s
              val chunk = stampeds take 2
              chunk foreach history.handleFatEvent
              lastEventId = chunk.last.eventId
              chunk collectFirst { case Stamped(eventId, _, KeyedEvent(_, _: OrderFinishedFat)) ⇒
                finished = true
                finishedEventId = eventId  // EventId of the first Master run (MasterReady of second Master run follows)
              }
              fatEvents ++= chunk map (_.value)
            }
            assert(rounds > 2)
            assert(history.orderEntries.map(normalizeTimestampsInEntry) ==
              expectedOrderEntries(runningAgents map (_.localUri.toString)))

            assert(listJournalFiles.length == 2 && listJournalFiles.contains("master--0.journal"))

            masterApi.executeCommand(MasterCommand.KeepEvents(finishedEventId - 1)) await 99.s
            assert(listJournalFiles.length == 2 && listJournalFiles.contains("master--0.journal"))  // Nothing deleted

            masterApi.executeCommand(MasterCommand.KeepEvents(finishedEventId)) await 99.s
            assert(listJournalFiles.length == 1 && !listJournalFiles.contains("master--0.journal"))  // First file deleted
            keepEventsEventId = finishedEventId
          }
        }
        val aAgentUri = runningAgents(0).localUri.toString
        val bAgentUri = runningAgents(1).localUri.toString
        assert(fatEvents.toSet == Set(
          NoKey <-: MasterReadyFat(MasterId("Master"), ZoneId.systemDefault.getId),
          OrderId("🔺") <-: OrderAddedFat(TestWorkflowId,None,Map("VARIABLE" → "VALUE")),
          AAgentPath <-: AgentReadyFat(ZoneId.systemDefault.getId),
          BAgentPath <-: AgentReadyFat(ZoneId.systemDefault.getId),
          OrderId("🔺") <-: OrderProcessingStartedFat(TestWorkflowId, AAgentPath, aAgentUri, jobName = None, Map("VARIABLE" → "VALUE")),
          OrderId("🔺") <-: OrderStdoutWrittenFat(StdoutOutput),
          OrderId("🔺") <-: OrderProcessedFat(Succeeded(ReturnCode(0)),Map("VARIABLE" → "VALUE")),
          OrderId("🔺") <-: OrderForkedFat(
            TestWorkflowId /: Position(1),Vector(
              OrderForkedFat.Child("🥕",OrderId("🔺/🥕"), Map("VARIABLE" → "VALUE")),
              OrderForkedFat.Child("🍋",OrderId("🔺/🍋"), Map("VARIABLE" → "VALUE")))),
          OrderId("🔺/🥕") <-: OrderProcessingStartedFat(TestWorkflowId /: Position(1, BranchId("🥕"), 0), AAgentPath, aAgentUri, jobName = None, Map("VARIABLE" → "VALUE")),
          OrderId("🔺/🍋") <-: OrderProcessingStartedFat(TestWorkflowId /: Position(1, BranchId("🍋"), 0), AAgentPath, aAgentUri, jobName = None, Map("VARIABLE" → "VALUE")),
          OrderId("🔺/🥕") <-: OrderStdoutWrittenFat(StdoutOutput),
          OrderId("🔺/🍋") <-: OrderStdoutWrittenFat(StdoutOutput),
          OrderId("🔺/🥕") <-: OrderProcessedFat(Succeeded(ReturnCode(0)),Map("VARIABLE" → "VALUE")),
          OrderId("🔺/🍋") <-: OrderProcessedFat(Succeeded(ReturnCode(0)),Map("VARIABLE" → "VALUE")),
          OrderId("🔺/🥕") <-: OrderProcessingStartedFat(TestWorkflowId /: Position(1, BranchId("🥕"), 1), AAgentPath, aAgentUri, jobName = None, Map("VARIABLE" → "VALUE")),
          OrderId("🔺/🍋") <-: OrderProcessingStartedFat(TestWorkflowId /: Position(1, BranchId("🍋"), 1), BAgentPath, bAgentUri, jobName = None, Map("VARIABLE" → "VALUE")),
          OrderId("🔺/🥕") <-: OrderStdoutWrittenFat(StdoutOutput),
          OrderId("🔺/🍋") <-: OrderStdoutWrittenFat(StdoutOutput),
          OrderId("🔺/🥕") <-: OrderProcessedFat(Succeeded(ReturnCode(0)),Map("VARIABLE" → "VALUE")),
          OrderId("🔺/🍋") <-: OrderProcessedFat(Succeeded(ReturnCode(0)),Map("VARIABLE" → "VALUE")),
          OrderId("🔺") <-: OrderJoinedFat(Vector(OrderId("🔺/🥕"), OrderId("🔺/🍋")), Map("VARIABLE" → "VALUE"), Succeeded(ReturnCode(0))),
          OrderId("🔺") <-: OrderProcessingStartedFat(TestWorkflowId /: Position(2), AAgentPath, aAgentUri, jobName = None, Map("VARIABLE" → "VALUE")),
          OrderId("🔺") <-: OrderStdoutWrittenFat(StdoutOutput),
          OrderId("🔺") <-: OrderProcessedFat(Succeeded(ReturnCode(0)),Map("VARIABLE" → "VALUE")),
          OrderId("🔺") <-: OrderFinishedFat(TestWorkflowId /: Position(3)),
          NoKey <-: MasterReadyFat(MasterId("Master"), ZoneId.systemDefault.getId)))

        provider.runMaster() { master ⇒
          autoClosing(new AkkaHttpMasterApi(master.localUri)) { masterApi ⇒
            // Test recovering FatState from snapshot stored in journal file
            assert(listJournalFiles.size == 2 && !listJournalFiles.contains("master--0.journal"))  // First file deleted
            masterApi.login(Some(UserAndPassword(UserId("TEST-USER"), SecretString("TEST-PASSWORD")))) await 99.s

            // after=0 is torn
            val torn = masterApi.fatEvents(EventRequest.singleClass[FatEvent](after = EventId.BeforeFirst, timeout = 99.seconds)) await 99.s
            assert(torn.isInstanceOf[TearableEventSeq.Torn])

            val EventSeq.NonEmpty(stampeds) = masterApi.fatEvents(EventRequest.singleClass[FatEvent](after = keepEventsEventId, timeout = 99.seconds)) await 99.s
            assert(stampeds.head.eventId > keepEventsEventId)
            assert(stampeds.map(_.value.event) ==
              Vector.fill(listJournalFiles.size)(MasterReadyFat(MasterId("Master"), ZoneId.systemDefault.getId)))  // Only MasterReady, nothing else happened

            locally { // Test a bug: Start a new journal file, then KeepEvent, then fetch fat events, while lean events invisible for fat events are issued
              assert(listJournalFiles.length == 2)
              val EventSeq.Empty(eventId1) = masterApi.fatEvents(EventRequest.singleClass[FatEvent](after = stampeds.last.eventId, timeout = 0.seconds)) await 99.s

              // Issue an event ignored by FatState
              masterApi.executeCommand(MasterCommand.IssueTestEvent) await 99.s

              // FatState should advance to the EventId returned as EventSeq.Empty (skipIgnoredEventIds)
              val EventSeq.Empty(eventId2) = masterApi.fatEvents(EventRequest.singleClass[FatEvent](after = eventId1, timeout = 0.seconds)) await 99.s
              assert(eventId2 > eventId1)

              // KeepEvents deletes last file
              masterApi.executeCommand(MasterCommand.KeepEvents(eventId2)) await 99.s
              assert(listJournalFiles.length == 1)  // One file deleted

              // Should not return Torn:
              val EventSeq.Empty(`eventId2`) = masterApi.fatEvents(EventRequest.singleClass[FatEvent](after = eventId2, timeout = 0.seconds)) await 99.s

              // Again, issue an ignored event. Then fetch fatEvents again after=eventId2 the second time
              masterApi.executeCommand(MasterCommand.IssueTestEvent) await 99.s
              val EventSeq.Empty(eventId3) = masterApi.fatEvents(EventRequest.singleClass[FatEvent](after = eventId2, timeout = 0.seconds)) await 99.s
              assert(eventId3 > eventId2)

              // Again, issue an ignored event. Then fetch fatEvents again after=eventId2 the third time
              masterApi.executeCommand(MasterCommand.IssueTestEvent) await 99.s
              val EventSeq.Empty(eventId4) = masterApi.fatEvents(EventRequest.singleClass[FatEvent](after = eventId2, timeout = 0.seconds)) await 99.s
              assert(eventId4 > eventId2)
            }
          }
        }
      }
    }
  }
}

object FatEventsTest
{
  private val AAgentPath = AgentPath("/AGENT-A")
  private val BAgentPath = AgentPath("/AGENT-B")
  private val TestExecutablePath = ExecutablePath(s"/TEST$sh")
  private val TestWorkflowId = WorkflowPath("/WORKFLOW") % "(initial)"
  private val TestWorkflowNotation = s"""
     |define workflow {
     |  execute executable="/TEST$sh", agent="AGENT-A";
     |  fork(
     |    "🥕" {
     |      execute executable="/TEST$sh", agent="AGENT-A";
     |      execute executable="/TEST$sh", agent="AGENT-A";
     |    },
     |    "🍋" {
     |      execute executable="/TEST$sh", agent="AGENT-A";
     |      execute executable="/TEST$sh", agent="AGENT-B";
     |    });
     |  execute executable="/TEST$sh", agent="AGENT-A";
     |}
     """.stripMargin.trim

  private val TestOrder = FreshOrder(OrderId("🔺"), TestWorkflowId.path, payload = Payload(Map("VARIABLE" → "VALUE")))
  private val TestTimestamp = Timestamp.Epoch

  private def expectedOrderEntries(agentUris: IndexedSeq[String]) = {
    implicit def toSome[A](a: A): Option[A] = Some(a)
    Vector(
      OrderEntry(
        TestOrder.id, None, Map("VARIABLE" → "VALUE"), OrderEntry.Cause.Added, TestWorkflowId /: Position(0), None, TestTimestamp, finishedAt = TestTimestamp, TestWorkflowId /: Position(3),
        Vector(
          OrderStepEntry(TestOrder.id, TestWorkflowId /: Position(0), agentUri = agentUris(0), jobName = None, Map("VARIABLE" → "VALUE"), TestTimestamp, TestTimestamp, ReturnCode(0), Map("VARIABLE" → "VALUE"), s"stdout: $StdoutOutput"),
          OrderStepEntry(OrderId("🔺"), TestWorkflowId /: Position(2), agentUris(0), jobName = None, Map("VARIABLE" → "VALUE"), TestTimestamp, TestTimestamp, ReturnCode(0), Map("VARIABLE" → "VALUE"), s"stdout: $StdoutOutput"))),
      OrderEntry(OrderId("🔺/🥕"), OrderId("🔺"), Map("VARIABLE" → "VALUE"), OrderEntry.Cause.Forked, TestWorkflowId /: Position(1, "🥕", 0), None, TestTimestamp, finishedAt = Some(TestTimestamp), None,
        steps = Vector(
          OrderStepEntry(OrderId("🔺/🥕"), TestWorkflowId /: Position(1, "🥕", 0), agentUris(0), jobName = None, Map("VARIABLE" → "VALUE"), TestTimestamp, TestTimestamp, ReturnCode(0), Map("VARIABLE" → "VALUE"), s"stdout: $StdoutOutput"),
          OrderStepEntry(OrderId("🔺/🥕"), TestWorkflowId /: Position(1, "🥕", 1), agentUris(0), jobName = None, Map("VARIABLE" → "VALUE"), TestTimestamp, TestTimestamp, ReturnCode(0), Map("VARIABLE" → "VALUE"), s"stdout: $StdoutOutput"))),
      OrderEntry(OrderId("🔺/🍋"), OrderId("🔺"), Map("VARIABLE" → "VALUE"), OrderEntry.Cause.Forked, TestWorkflowId /: Position(1, "🍋", 0), None, TestTimestamp, finishedAt = Some(TestTimestamp), None,
        steps = Vector(
          OrderStepEntry(OrderId("🔺/🍋"), TestWorkflowId /: Position(1, "🍋", 0), agentUris(0), jobName = None, Map("VARIABLE" → "VALUE"), TestTimestamp, TestTimestamp, ReturnCode(0), Map("VARIABLE" → "VALUE"), s"stdout: $StdoutOutput"),
          OrderStepEntry(OrderId("🔺/🍋"), TestWorkflowId /: Position(1, "🍋", 1), agentUris(1), jobName = None, Map("VARIABLE" → "VALUE"), TestTimestamp, TestTimestamp, ReturnCode(0), Map("VARIABLE" → "VALUE"), s"stdout: $StdoutOutput"))))
    }

  private def normalizeTimestampsInEntry(entry: OrderEntry): OrderEntry =
    entry.copy(
      startedAt = entry.startedAt map (_ ⇒ TestTimestamp),
      scheduledFor = entry.scheduledFor map (_ ⇒ TestTimestamp),
      finishedAt = entry.finishedAt map (_ ⇒ TestTimestamp),
      steps = entry.steps map normalizeTimestampsInStep)

  private def normalizeTimestampsInStep(step: OrderStepEntry): OrderStepEntry =
    step.copy(
      startedAt = TestTimestamp,
      endedAt = step.endedAt map (_ ⇒ TestTimestamp))
}
