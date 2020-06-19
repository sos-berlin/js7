package js7.tests.history

import java.time.ZoneId
import js7.base.auth.{UserAndPassword, UserId}
import js7.base.generic.SecretString
import js7.base.problem.Checked.Ops
import js7.base.time.ScalaTime._
import js7.base.time.Timestamp
import js7.base.utils.AutoClosing.autoClosing
import js7.base.web.Uri
import js7.common.process.Processes.{ShellFileExtension => sh}
import js7.common.scalautil.FileUtils.syntax.RichPath
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.time.WaitForCondition.waitForCondition
import js7.controller.data.ControllerCommand
import js7.controller.data.ControllerCommand.NoOperation
import js7.core.event.journal.files.JournalFiles
import js7.data.agent.AgentRefPath
import js7.data.controller.ControllerId
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{EventId, EventRequest, EventSeq, KeyedEvent, Stamped, TearableEventSeq}
import js7.data.fatevent.AgentFatEvent.AgentReadyFat
import js7.data.fatevent.ControllerFatEvent.ControllerReadyFat
import js7.data.fatevent.FatEvent
import js7.data.fatevent.OrderFatEvent.{OrderAddedFat, OrderFinishedFat, OrderForkedFat, OrderJoinedFat, OrderProcessedFat, OrderProcessingStartedFat, OrderStdoutWrittenFat}
import js7.data.job.{ExecutablePath, ReturnCode}
import js7.data.order.OrderEvent.OrderFinished
import js7.data.order.Outcome.Succeeded
import js7.data.order.{FreshOrder, OrderId}
import js7.data.workflow.WorkflowPath
import js7.data.workflow.parser.WorkflowParser
import js7.data.workflow.position.Position
import js7.tests.history.FatEventsTest._
import js7.tests.testenv.DirectoryProvider
import js7.tests.testenv.DirectoryProvider.StdoutOutput
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import scala.collection.mutable
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
final class FatEventsTest extends AnyFreeSpec
{
  "test" in {
    autoClosing(new DirectoryProvider(AAgentRefPath :: BAgentRefPath :: Nil, TestWorkflow :: Nil, testName = Some("FatEventsTest"))) { provider =>
      (provider.controller.configDir / "private/private.conf").append("""
        |js7.auth.users.TEST-USER = "plain:TEST-PASSWORD"
        |js7.journal.users-allowed-to-release-events = [ "TEST-USER" ]
        |""".stripMargin )
      for (a <- provider.agents) a.writeExecutable(TestExecutablePath, DirectoryProvider.script(0.s))

      def listJournalFiles = JournalFiles.listJournalFiles(provider.controller.dataDir / "state" / "controller").map(_.file.getFileName.toString)

      def assertJournalFileCount(n: Int): Unit = {
        waitForCondition(9.s, 10.ms) { listJournalFiles.size == n }
        assert(listJournalFiles.size == n)
      }

      provider.runAgents() { runningAgents =>
        provider.runController() { controller =>
          controller.httpApi.login_(Some(UserAndPassword(UserId("TEST-USER"), SecretString("TEST-PASSWORD")))) await 99.s
          controller.addOrderBlocking(TestOrder)
          controller.eventWatch.await[OrderFinished](_.key == TestOrder.id)
          assert(listJournalFiles == Vector("controller--0.journal"))

          val request = EventRequest.singleClass[FatEvent](after = EventId.BeforeFirst, timeout = Some(99.s), limit = 2)
          val EventSeq.NonEmpty(stampedEvents) = controller.httpApi.fatEvents(request) await 99.s
          assert(stampedEvents.map(_.value).take(2) == Vector(
            NoKey <-: ControllerReadyFat(ControllerId("Controller"), ZoneId.systemDefault.getId),
            OrderId("🔺") <-: OrderAddedFat(TestWorkflowId, None, Map("KEY" -> "VALUE"))))

          val EventSeq.NonEmpty(stampedEvents1) = controller.httpApi.fatEvents(request.copy[FatEvent](after = stampedEvents.head.eventId)) await 99.s
          assert(stampedEvents1.head == stampedEvents(1))
        }
        assertJournalFileCount(2)  // Controller shutdown added a journal file
        var releaseEventsEventId = EventId.BeforeFirst
        var lastAddedEventId = EventId.BeforeFirst
        val fatEvents = mutable.Buffer[KeyedEvent[FatEvent]]()
        provider.runController() { controller =>
          controller.httpApi.login_(Some(UserAndPassword(UserId("TEST-USER"), SecretString("TEST-PASSWORD")))) await 99.s
          val history = new InMemoryHistory
          var lastEventId = EventId.BeforeFirst
          var finished = false
          var finishedEventId = -1L
          var rounds = 0
          while (!finished) {
            rounds += 1
            val request = EventRequest.singleClass[FatEvent](after = lastEventId, timeout = Some(99.s), limit = 2)
            val EventSeq.NonEmpty(stampedEvents) = controller.httpApi.fatEvents(request) await 99.s
            val chunk = stampedEvents take 2
            chunk foreach history.handleFatEvent
            lastEventId = chunk.last.eventId
            chunk collectFirst { case Stamped(eventId, _, KeyedEvent(_, _: OrderFinishedFat)) =>
              finished = true
              finishedEventId = eventId  // EventId of the first Controller run (ControllerReady of second Controller run follows)
            }
            fatEvents ++= chunk map (_.value)
          }
          assert(rounds > 2)
          assert(history.orderEntries.map(normalizeTimestampsInEntry) ==
            expectedOrderEntries(runningAgents.map(_.localUri)))

          assertJournalFileCount(3)
          assert(listJournalFiles.contains("controller--0.journal"))

          controller.httpApi.executeCommand(ControllerCommand.ReleaseEvents(finishedEventId - 1)) await 99.s
          assertJournalFileCount(3)
          assert(listJournalFiles.contains("controller--0.journal"))  // Nothing deleted

          controller.httpApi.executeCommand(ControllerCommand.ReleaseEvents(finishedEventId)) await 99.s
          assertJournalFileCount(2)
          assert(!listJournalFiles.contains("controller--0.journal"))  // First file deleted
          releaseEventsEventId = finishedEventId
          lastAddedEventId = controller.eventWatch.lastAddedEventId
        }
        assertJournalFileCount(3)  // Controller shutdown added a journal file
        val aAgentUri = runningAgents(0).localUri
        val bAgentUri = runningAgents(1).localUri
        assert(fatEvents.toSet == Set(
          NoKey <-: ControllerReadyFat(ControllerId("Controller"), ZoneId.systemDefault.getId),
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
          NoKey <-: ControllerReadyFat(ControllerId("Controller"), ZoneId.systemDefault.getId)))

        provider.runController() { controller =>
          // Wait until ControllerOrderKeeper has become ready
          controller.executeCommandAsSystemUser(NoOperation).await(99.s).orThrow
          // Test recovering FatState from snapshot stored in journal file
          assertJournalFileCount(4)
          assert( !listJournalFiles.contains("controller--0.journal"))  // First file deleted
          controller.httpApi.login_(Some(UserAndPassword(UserId("TEST-USER"), SecretString("TEST-PASSWORD")))) await 99.s

          controller.waitUntilReady()

          // after=0 is torn
          val torn = controller.httpApi.fatEvents(EventRequest.singleClass[FatEvent](after = EventId.BeforeFirst, timeout = Some(99.s))) await 99.s
          assert(torn.isInstanceOf[TearableEventSeq.Torn])

          val EventSeq.NonEmpty(stamped) = controller.httpApi.fatEvents(EventRequest.singleClass[FatEvent](after = releaseEventsEventId, timeout = Some(99.s))) await 99.s
          assert(stamped.head.eventId > releaseEventsEventId)
          assert(stamped.map(_.value.event) ==
            Vector.fill(2)(ControllerReadyFat(ControllerId("Controller"), ZoneId.systemDefault.getId)))  // Only ControllerReady, nothing else happened

          locally { // Test a bug: Start a new journal file, then KeepEvent, then fetch fat events, while lean events invisible for fat events are emitted
            assertJournalFileCount(4)
            val EventSeq.Empty(eventId1) = controller.httpApi.fatEvents(EventRequest.singleClass[FatEvent](after = stamped.last.eventId, timeout = Some(0.s))) await 99.s

            // Emit an event ignored by FatState
            controller.httpApi.executeCommand(ControllerCommand.EmitTestEvent) await 99.s

            // FatState should advance to the EventId returned as EventSeq.Empty (skipIgnoredEventIds)
            val EventSeq.Empty(eventId2) = controller.httpApi.fatEvents(EventRequest.singleClass[FatEvent](after = eventId1, timeout = Some(0.s))) await 99.s
            assert(eventId2 > eventId1)

            // ReleaseEvents deletes last file
            controller.httpApi.executeCommand(ControllerCommand.ReleaseEvents(eventId2)) await 99.s
            assertJournalFileCount(1)  // One file deleted

            // Should not return Torn:
            val EventSeq.Empty(_) = controller.httpApi.fatEvents(EventRequest.singleClass[FatEvent](after = eventId2, timeout = Some(0.s))) await 99.s

            // Again, emit an ignored event. Then fetch fatEvents again after=eventId2 the second time
            controller.httpApi.executeCommand(ControllerCommand.EmitTestEvent) await 99.s
            val EventSeq.Empty(eventId3) = controller.httpApi.fatEvents(EventRequest.singleClass[FatEvent](after = eventId2, timeout = Some(0.s))) await 99.s
            assert(eventId3 > eventId2)

            // Again, emit an ignored event. Then fetch fatEvents again after=eventId2 the third time
            controller.httpApi.executeCommand(ControllerCommand.EmitTestEvent) await 99.s
            val EventSeq.Empty(eventId4) = controller.httpApi.fatEvents(EventRequest.singleClass[FatEvent](after = eventId2, timeout = Some(0.s))) await 99.s
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
