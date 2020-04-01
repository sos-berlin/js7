package com.sos.jobscheduler.tests.history

import com.sos.jobscheduler.base.auth.{UserAndPassword, UserId}
import com.sos.jobscheduler.base.generic.SecretString
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.time.Timestamp
import com.sos.jobscheduler.base.utils.AutoClosing.autoClosing
import com.sos.jobscheduler.base.web.Uri
import com.sos.jobscheduler.common.process.Processes.{ShellFileExtension => sh}
import com.sos.jobscheduler.common.scalautil.FileUtils.syntax.RichPath
import com.sos.jobscheduler.common.scalautil.MonixUtils.syntax._
import com.sos.jobscheduler.core.event.journal.files.JournalFiles
import com.sos.jobscheduler.data.agent.AgentRefPath
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
import com.sos.jobscheduler.data.order.{FreshOrder, OrderId}
import com.sos.jobscheduler.data.workflow.WorkflowPath
import com.sos.jobscheduler.data.workflow.parser.WorkflowParser
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.master.data.MasterCommand
import com.sos.jobscheduler.master.data.MasterCommand.NoOperation
import com.sos.jobscheduler.tests.history.FatEventsTest._
import com.sos.jobscheduler.tests.testenv.DirectoryProvider
import com.sos.jobscheduler.tests.testenv.DirectoryProvider.StdoutOutput
import java.time.ZoneId
import monix.execution.Scheduler.Implicits.global
import org.scalatest.FreeSpec
import scala.collection.immutable.IndexedSeq
import scala.collection.mutable
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
final class FatEventsTest extends FreeSpec
{
  "test" in {
    autoClosing(new DirectoryProvider(AAgentRefPath :: BAgentRefPath :: Nil, TestWorkflow :: Nil, testName = Some("FatEventsTest"))) { provider =>
      (provider.master.configDir / "private/private.conf").append("""
        |jobscheduler.auth.users.TEST-USER = "plain:TEST-PASSWORD"
        |""".stripMargin )
      for (a <- provider.agents) a.writeExecutable(TestExecutablePath, DirectoryProvider.script(0.s))

      def listJournalFiles = JournalFiles.listJournalFiles(provider.master.dataDir / "state" / "master").map(_.file.getFileName.toString)

      provider.runAgents() { runningAgents =>
        provider.runMaster() { master =>
          master.httpApi.login(Some(UserAndPassword(UserId("TEST-USER"), SecretString("TEST-PASSWORD")))) await 99.s
          master.addOrderBlocking(TestOrder)
          master.eventWatch.await[OrderFinished](_.key == TestOrder.id)
          assert(listJournalFiles == Vector("master--0.journal"))

          val request = EventRequest.singleClass[FatEvent](after = EventId.BeforeFirst, timeout = Some(99.s), limit = 2)
          val EventSeq.NonEmpty(stampedEvents) = master.httpApi.fatEvents(request) await 99.s
          assert(stampedEvents.map(_.value).take(2) == Vector(
            NoKey <-: MasterReadyFat(MasterId("Master"), ZoneId.systemDefault.getId),
            OrderId("🔺") <-: OrderAddedFat(TestWorkflowId, None, Map("KEY" -> "VALUE"))))

          val EventSeq.NonEmpty(stampedEvents1) = master.httpApi.fatEvents(request.copy[FatEvent](after = stampedEvents.head.eventId)) await 99.s
          assert(stampedEvents1.head == stampedEvents(1))
        }
        assert(listJournalFiles.size == 2)  // Master shutdown added a journal file
        var keepEventsEventId = EventId.BeforeFirst
        var lastAddedEventId = EventId.BeforeFirst
        val fatEvents = mutable.Buffer[KeyedEvent[FatEvent]]()
        provider.runMaster() { master =>
          master.httpApi.login(Some(UserAndPassword(UserId("TEST-USER"), SecretString("TEST-PASSWORD")))) await 99.s
          val history = new InMemoryHistory
          var lastEventId = EventId.BeforeFirst
          var finished = false
          var finishedEventId = -1L
          var rounds = 0
          while (!finished) {
            rounds += 1
            val request = EventRequest.singleClass[FatEvent](after = lastEventId, timeout = Some(99.s), limit = 2)
            val EventSeq.NonEmpty(stampedEvents) = master.httpApi.fatEvents(request) await 99.s
            val chunk = stampedEvents take 2
            chunk foreach history.handleFatEvent
            lastEventId = chunk.last.eventId
            chunk collectFirst { case Stamped(eventId, _, KeyedEvent(_, _: OrderFinishedFat)) =>
              finished = true
              finishedEventId = eventId  // EventId of the first Master run (MasterReady of second Master run follows)
            }
            fatEvents ++= chunk map (_.value)
          }
          assert(rounds > 2)
          assert(history.orderEntries.map(normalizeTimestampsInEntry) ==
            expectedOrderEntries(runningAgents.map(_.localUri)))

          assert(listJournalFiles.size == 3 && listJournalFiles.contains("master--0.journal"))

          master.httpApi.executeCommand(MasterCommand.KeepEvents(finishedEventId - 1)) await 99.s
          assert(listJournalFiles.size == 3 && listJournalFiles.contains("master--0.journal"))  // Nothing deleted

          master.httpApi.executeCommand(MasterCommand.KeepEvents(finishedEventId)) await 99.s
          assert(listJournalFiles.size == 2 && !listJournalFiles.contains("master--0.journal"))  // First file deleted
          keepEventsEventId = finishedEventId
          lastAddedEventId = master.eventWatch.lastAddedEventId
        }
        assert(listJournalFiles.size == 3)  // Master shutdown added a journal file
        val aAgentUri = runningAgents(0).localUri
        val bAgentUri = runningAgents(1).localUri
        assert(fatEvents.toSet == Set(
          NoKey <-: MasterReadyFat(MasterId("Master"), ZoneId.systemDefault.getId),
          OrderId("🔺") <-: OrderAddedFat(TestWorkflowId, None, Map("KEY" -> "VALUE")),
          AAgentRefPath <-: AgentReadyFat(ZoneId.systemDefault.getId),
          BAgentRefPath <-: AgentReadyFat(ZoneId.systemDefault.getId),
          OrderId("🔺") <-: OrderProcessingStartedFat(TestWorkflowId, AAgentRefPath, aAgentUri, jobName = None, Map("KEY" -> "VALUE")),
          OrderId("🔺") <-: OrderStdoutWrittenFat(StdoutOutput),
          OrderId("🔺") <-: OrderProcessedFat(Succeeded(ReturnCode(0)),Map("KEY" -> "VALUE")),
          OrderId("🔺") <-: OrderForkedFat(
            TestWorkflowId /: Position(1),Vector(
              OrderForkedFat.Child("🥕",OrderId("🔺/🥕"), Map("KEY" -> "VALUE")),
              OrderForkedFat.Child("🍋",OrderId("🔺/🍋"), Map("KEY" -> "VALUE")))),
          OrderId("🔺/🥕") <-: OrderProcessingStartedFat(TestWorkflowId /: (Position(1) / "fork+🥕" % 0), AAgentRefPath, aAgentUri, jobName = None, Map("KEY" -> "VALUE")),
          OrderId("🔺/🍋") <-: OrderProcessingStartedFat(TestWorkflowId /: (Position(1) / "fork+🍋" % 0), AAgentRefPath, aAgentUri, jobName = None, Map("KEY" -> "VALUE")),
          OrderId("🔺/🥕") <-: OrderStdoutWrittenFat(StdoutOutput),
          OrderId("🔺/🍋") <-: OrderStdoutWrittenFat(StdoutOutput),
          OrderId("🔺/🥕") <-: OrderProcessedFat(Succeeded(ReturnCode(0)), Map("KEY" -> "VALUE")),
          OrderId("🔺/🍋") <-: OrderProcessedFat(Succeeded(ReturnCode(0)), Map("KEY" -> "VALUE")),
          OrderId("🔺/🥕") <-: OrderProcessingStartedFat(TestWorkflowId /: (Position(1) / "fork+🥕" % 1), AAgentRefPath, aAgentUri, jobName = None, Map("KEY" -> "VALUE")),
          OrderId("🔺/🍋") <-: OrderProcessingStartedFat(TestWorkflowId /: (Position(1) / "fork+🍋" % 1), BAgentRefPath, bAgentUri, jobName = None, Map("KEY" -> "VALUE")),
          OrderId("🔺/🥕") <-: OrderStdoutWrittenFat(StdoutOutput),
          OrderId("🔺/🍋") <-: OrderStdoutWrittenFat(StdoutOutput),
          OrderId("🔺/🥕") <-: OrderProcessedFat(Succeeded(ReturnCode(0)), Map("KEY" -> "VALUE")),
          OrderId("🔺/🍋") <-: OrderProcessedFat(Succeeded(ReturnCode(0)), Map("KEY" -> "VALUE")),
          OrderId("🔺") <-: OrderJoinedFat(Vector(OrderId("🔺/🥕"), OrderId("🔺/🍋")), Succeeded(ReturnCode(0))),
          OrderId("🔺") <-: OrderProcessingStartedFat(TestWorkflowId /: Position(2), AAgentRefPath, aAgentUri, jobName = None, Map("KEY" -> "VALUE")),
          OrderId("🔺") <-: OrderStdoutWrittenFat(StdoutOutput),
          OrderId("🔺") <-: OrderProcessedFat(Succeeded(ReturnCode(0)), Map("KEY" -> "VALUE")),
          OrderId("🔺") <-: OrderFinishedFat(TestWorkflowId /: Position(3)),
          NoKey <-: MasterReadyFat(MasterId("Master"), ZoneId.systemDefault.getId)))

        provider.runMaster() { master =>
          // Wait until MasterOrderKeeper has become ready
          master.executeCommandAsSystemUser(NoOperation).await(99.s).orThrow
          // Test recovering FatState from snapshot stored in journal file
          assert(listJournalFiles.size == 4 && !listJournalFiles.contains("master--0.journal"))  // First file deleted
          master.httpApi.login(Some(UserAndPassword(UserId("TEST-USER"), SecretString("TEST-PASSWORD")))) await 99.s

          master.waitUntilReady()

          // after=0 is torn
          val torn = master.httpApi.fatEvents(EventRequest.singleClass[FatEvent](after = EventId.BeforeFirst, timeout = Some(99.s))) await 99.s
          assert(torn.isInstanceOf[TearableEventSeq.Torn])

          val EventSeq.NonEmpty(stamped) = master.httpApi.fatEvents(EventRequest.singleClass[FatEvent](after = keepEventsEventId, timeout = Some(99.s))) await 99.s
          assert(stamped.head.eventId > keepEventsEventId)
          assert(stamped.map(_.value.event) ==
            Vector.fill(2)(MasterReadyFat(MasterId("Master"), ZoneId.systemDefault.getId)))  // Only MasterReady, nothing else happened

          locally { // Test a bug: Start a new journal file, then KeepEvent, then fetch fat events, while lean events invisible for fat events are issued
            assert(listJournalFiles.size == 4)
            val EventSeq.Empty(eventId1) = master.httpApi.fatEvents(EventRequest.singleClass[FatEvent](after = stamped.last.eventId, timeout = Some(0.s))) await 99.s

            // Issue an event ignored by FatState
            master.httpApi.executeCommand(MasterCommand.IssueTestEvent) await 99.s

            // FatState should advance to the EventId returned as EventSeq.Empty (skipIgnoredEventIds)
            val EventSeq.Empty(eventId2) = master.httpApi.fatEvents(EventRequest.singleClass[FatEvent](after = eventId1, timeout = Some(0.s))) await 99.s
            assert(eventId2 > eventId1)

            // KeepEvents deletes last file
            master.httpApi.executeCommand(MasterCommand.KeepEvents(eventId2)) await 99.s
            assert(listJournalFiles.size == 1)  // One file deleted

            // Should not return Torn:
            val EventSeq.Empty(`eventId2`) = master.httpApi.fatEvents(EventRequest.singleClass[FatEvent](after = eventId2, timeout = Some(0.s))) await 99.s

            // Again, issue an ignored event. Then fetch fatEvents again after=eventId2 the second time
            master.httpApi.executeCommand(MasterCommand.IssueTestEvent) await 99.s
            val EventSeq.Empty(eventId3) = master.httpApi.fatEvents(EventRequest.singleClass[FatEvent](after = eventId2, timeout = Some(0.s))) await 99.s
            assert(eventId3 > eventId2)

            // Again, issue an ignored event. Then fetch fatEvents again after=eventId2 the third time
            master.httpApi.executeCommand(MasterCommand.IssueTestEvent) await 99.s
            val EventSeq.Empty(eventId4) = master.httpApi.fatEvents(EventRequest.singleClass[FatEvent](after = eventId2, timeout = Some(0.s))) await 99.s
            assert(eventId4 > eventId2)
          }
        }
      }
    }
  }
}

object FatEventsTest
{
  private val AAgentRefPath = AgentRefPath("/AGENT-A")
  private val BAgentRefPath = AgentRefPath("/AGENT-B")
  private val TestExecutablePath = ExecutablePath(s"/TEST$sh")
  private val TestWorkflowId = WorkflowPath("/WORKFLOW") ~ "INITIAL"
  private val TestWorkflow = WorkflowParser.parse(TestWorkflowId, s"""
     |define workflow {
     |  execute executable="/TEST$sh", agent="AGENT-A";
     |  fork {
     |    "🥕": {
     |      execute executable="/TEST$sh", agent="AGENT-A";
     |      execute executable="/TEST$sh", agent="AGENT-A";
     |    },
     |    "🍋": {
     |      execute executable="/TEST$sh", agent="AGENT-A";
     |      execute executable="/TEST$sh", agent="AGENT-B";
     |    }
     |  }
     |  execute executable="/TEST$sh", agent="AGENT-A";
     |}
     """.stripMargin.trim).orThrow

  private val TestOrder = FreshOrder(OrderId("🔺"), TestWorkflowId.path, arguments = Map("KEY" -> "VALUE"))
  private val TestTimestamp = Timestamp.Epoch

  private def expectedOrderEntries(agentUris: IndexedSeq[Uri]) = {
    implicit def toSome[A](a: A): Option[A] = Some(a)
    Vector(
      OrderEntry(
        TestOrder.id, None, Map("KEY" -> "VALUE"), OrderEntry.Cause.Added, TestWorkflowId /: Position(0), None, TestTimestamp, finishedAt = TestTimestamp, TestWorkflowId /: Position(3),
        Vector(
          OrderStepEntry(TestOrder.id, TestWorkflowId /: Position(0), agentUri = agentUris(0), jobName = None, Map("KEY" -> "VALUE"), TestTimestamp, TestTimestamp, ReturnCode(0), Map("KEY" -> "VALUE"), s"stdout: $StdoutOutput"),
          OrderStepEntry(OrderId("🔺"), TestWorkflowId /: Position(2), agentUris(0), jobName = None, Map("KEY" -> "VALUE"), TestTimestamp, TestTimestamp, ReturnCode(0), Map("KEY" -> "VALUE"), s"stdout: $StdoutOutput"))),
      OrderEntry(OrderId("🔺/🥕"), OrderId("🔺"), Map("KEY" -> "VALUE"), OrderEntry.Cause.Forked, TestWorkflowId /: (Position(1) / "fork+🥕" % 0), None, TestTimestamp, finishedAt = Some(TestTimestamp), None,
        steps = Vector(
          OrderStepEntry(OrderId("🔺/🥕"), TestWorkflowId /: (Position(1) / "fork+🥕" % 0), agentUris(0), jobName = None, Map("KEY" -> "VALUE"), TestTimestamp, TestTimestamp, ReturnCode(0), Map("KEY" -> "VALUE"), s"stdout: $StdoutOutput"),
          OrderStepEntry(OrderId("🔺/🥕"), TestWorkflowId /: (Position(1) / "fork+🥕" % 1), agentUris(0), jobName = None, Map("KEY" -> "VALUE"), TestTimestamp, TestTimestamp, ReturnCode(0), Map("KEY" -> "VALUE"), s"stdout: $StdoutOutput"))),
      OrderEntry(OrderId("🔺/🍋"), OrderId("🔺"), Map("KEY" -> "VALUE"), OrderEntry.Cause.Forked, TestWorkflowId /: (Position(1) / "fork+🍋" % 0), None, TestTimestamp, finishedAt = Some(TestTimestamp), None,
        steps = Vector(
          OrderStepEntry(OrderId("🔺/🍋"), TestWorkflowId /: (Position(1) / "fork+🍋" % 0), agentUris(0), jobName = None, Map("KEY" -> "VALUE"), TestTimestamp, TestTimestamp, ReturnCode(0), Map("KEY" -> "VALUE"), s"stdout: $StdoutOutput"),
          OrderStepEntry(OrderId("🔺/🍋"), TestWorkflowId /: (Position(1) / "fork+🍋" % 1), agentUris(1), jobName = None, Map("KEY" -> "VALUE"), TestTimestamp, TestTimestamp, ReturnCode(0), Map("KEY" -> "VALUE"), s"stdout: $StdoutOutput"))))
    }

  private def normalizeTimestampsInEntry(entry: OrderEntry): OrderEntry =
    entry.copy(
      startedAt = entry.startedAt map (_ => TestTimestamp),
      scheduledFor = entry.scheduledFor map (_ => TestTimestamp),
      finishedAt = entry.finishedAt map (_ => TestTimestamp),
      steps = entry.steps map normalizeTimestampsInStep)

  private def normalizeTimestampsInStep(step: OrderStepEntry): OrderStepEntry =
    step.copy(
      startedAt = TestTimestamp,
      endedAt = step.endedAt map (_ => TestTimestamp))
}
