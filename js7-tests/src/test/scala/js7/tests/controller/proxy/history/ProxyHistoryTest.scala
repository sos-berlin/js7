package js7.tests.controller.proxy.history

//diffx import com.softwaremill.diffx.generic.auto.*
//diffx import com.softwaremill.diffx.scalatest.DiffShouldMatcher.*
import org.apache.pekko
import js7.data.workflow.position.BranchPath.syntax.*
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.eventbus.StandardEventBus
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.log.Logger
import js7.base.problem.Checked.Ops
import js7.base.problem.ProblemException
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ScalaUtils.*
import js7.common.pekkoutils.ProvideActorSystem
import js7.data.Problems.SnapshotForUnknownEventIdProblem
import js7.data.agent.AgentPath
import js7.data.controller.ControllerCommand.TakeSnapshot
import js7.data.controller.ControllerEvent.ControllerReady
import js7.data.controller.ControllerState
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{EventId, KeyedEvent, Stamped}
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDetachable, OrderDetached, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStdoutWritten}
import js7.data.order.Outcome.{Succeeded, succeeded}
import js7.data.order.{FreshOrder, OrderEvent, OrderId}
import js7.data.value.{NamedValues, StringValue}
import js7.data.workflow.WorkflowParser
import js7.data.workflow.position.Position
import js7.data_for_java.auth.{JAdmission, JHttpsConfig}
import js7.journal.files.JournalFiles.listJournalFiles
import js7.proxy.data.event.{EventAndState, ProxyStarted}
import js7.proxy.javaapi.JProxyContext
import js7.tester.ScalaTestUtils.awaitAndAssert
import js7.tests.controller.proxy.ClusterProxyTest
import js7.tests.controller.proxy.history.JControllerApiHistoryTester.TestWorkflowId
import js7.tests.controller.proxy.history.ProxyHistoryTest.*
import js7.tests.testenv.ControllerClusterForScalaTest.TestPathExecutable
import js7.tests.testenv.DirectoryProvider.{StdoutOutput, toLocalSubagentId}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced
import org.scalactic.source
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

final class ProxyHistoryTest extends OurTestSuite, ProvideActorSystem, ClusterProxyTest:

  private val maxRounds = 100

  override protected def config = config"""
    pekko.http.host-connection-pool.max-connections = ${4 + maxRounds}
    """.withFallback(super.config)

  private val controllerConfig = config"""
    js7.proxy.torn-older = 0s  # Should be irrelevant
    js7.journal.users-allowed-to-release-events = [ "Proxy" ]
    js7.journal.release-events-delay = 0s
    """
  override protected def primaryControllerConfig = controllerConfig withFallback super.primaryControllerConfig
  override protected def backupControllerConfig = controllerConfig withFallback super.backupControllerConfig
  override protected val items = Seq(TestWorkflow)
  override protected val agentPaths = AAgentPath :: BAgentPath :: Nil

  "Read event stream in small parts and write history" in:
    withControllerAndBackup() { (primary, _, backup, _, _) =>
      def listJournalFilenames = listJournalFiles(primary.controllerEnv.dataDir / "state" / "controller")
        .map(_.file.getFileName.toString)

      def assertJournalFileCount(n: Int)(implicit pos: source.Position): Unit =
        awaitAndAssert { listJournalFilenames.size == n }

      val keyedEvents = mutable.Buffer[KeyedEvent[OrderEvent]]()

      runControllers(primary, backup) { (primaryController, _) =>
        controllerApi.executeCommand(TakeSnapshot).await(99.s).orThrow
        assertJournalFileCount(2)

        controllerApi.addOrder(TestOrder).await(99.s).orThrow
        val finishedEventId = primaryController.eventWatch.await[OrderFinished](_.key == TestOrder.id).head.eventId

        var releaseEventsEventId = EventId.BeforeFirst
        var lastAddedEventId = EventId.BeforeFirst
        @volatile var lastState = ControllerState.empty
        @volatile var finished = false
        var rounds = 0
        while !finished && rounds <= maxRounds do
          logger.info(s"Round $rounds")
          var proxyStartedReceived = false
          try
            controllerApi.eventAndStateObservable(new StandardEventBus, Some(lastState.eventId))
              .doOnNext(es => Task(logger.debug(s"observe ${es.stampedEvent}")))
              .takeWhileInclusive:
                case EventAndState(Stamped(_, _, KeyedEvent(TestOrder.id, _: OrderFinished)), _, _) =>
                  finished = true
                  false
                case _=>
                  true
              .take(3)  // Process two events (and initial ProxyStarted) each test round
              .doOnNext(es => Task {
                es.stampedEvent.value.event match {
                  case ProxyStarted =>
                    assert(!proxyStartedReceived)
                    proxyStartedReceived = true
                    //diffx es.state shouldMatchTo lastState
                    assert(es.state == lastState)
                  case _ =>
                    assert(lastState.eventId < es.stampedEvent.eventId)
                }
                lastState = es.state
                var keyedEvent = es.stampedEvent.value
                for controllerReady <- ifCast[ControllerReady](keyedEvent.event) do {
                  keyedEvent = NoKey <-: controllerReady.copy(totalRunningTime = 333.s)
                }
                es.stampedEvent.value match {
                  case KeyedEvent(orderId: OrderId, event: OrderEvent) => keyedEvents += orderId <-: event
                  case _ =>
                }
              })
              .completedL
              .await(99.s)
            assert(proxyStartedReceived)
          catch { case t @ pekko.stream.SubscriptionWithCancelException.NoMoreElementsNeeded =>
            // TODO NoMoreElementsNeeded occurs occasionally for unknown reason
            // Anyway, the caller should repeat the call.
            logger.error(s"Ignore ${t.toString}")
          }
          rounds += 1
        assert(rounds > 2)

        assertJournalFileCount(2)
        assert(listJournalFilenames.contains("controller--0.journal"))
        controllerApi.releaseEvents(finishedEventId).await(99.s).orThrow
        assertJournalFileCount(1)
        assert(!listJournalFilenames.contains("controller--0.journal"))  // First file deleted

        releaseEventsEventId = finishedEventId
        lastAddedEventId = primaryController.eventWatch.lastAddedEventId

        assert(keyedEvents.groupMap(_.key)(_.event).view.mapValues(_.toList).to(mutable.SortedMap) == mutable.SortedMap(
          OrderId("🔺") -> List(
            OrderAdded(TestWorkflowId.asScala, Map("KEY" -> StringValue("VALUE"))),
            OrderAttachable(AAgentPath),
            OrderAttached(AAgentPath),
            OrderStarted,
            OrderProcessingStarted(aSubagentId),
            OrderStdoutWritten(StdoutOutput),
            OrderProcessed(Succeeded(NamedValues.rc(0))),
            OrderMoved(Position(1)),
            OrderForked(Vector(
              "🥕" -> OrderId("🔺|🥕"),
              "🍋" -> OrderId("🔺|🍋"))),
            OrderDetachable,
            OrderDetached,
            OrderJoined(succeeded),
            OrderMoved(Position(2)),
            OrderAttachable(AAgentPath),
            OrderAttached(AAgentPath),
            OrderProcessingStarted(aSubagentId),
            OrderStdoutWritten(StdoutOutput),
            OrderProcessed(Succeeded(NamedValues.rc(0))),
            OrderMoved(Position(3)),
            OrderDetachable,
            OrderDetached,
            OrderFinished()),
          OrderId("🔺|🥕") -> List(
            OrderProcessingStarted(aSubagentId),
            OrderStdoutWritten(StdoutOutput),
            OrderProcessed(Succeeded(NamedValues.rc(0))),
            OrderMoved(Position(1) / "fork+🥕" % 1),
            OrderProcessingStarted(aSubagentId),
            OrderStdoutWritten(StdoutOutput),
            OrderProcessed(Succeeded(NamedValues.rc(0))),
            OrderMoved(Position(1) / "fork+🥕" % 2),
            OrderDetachable,
            OrderDetached),
          OrderId("🔺|🍋") -> List(
            OrderProcessingStarted(aSubagentId),
            OrderStdoutWritten(StdoutOutput),
            OrderProcessed(Succeeded(NamedValues.rc(0))),
            OrderMoved(Position(1) / "fork+🍋" % 1),
            OrderDetachable,
            OrderDetached,
            OrderAttachable(BAgentPath),
            OrderAttached(BAgentPath),
            OrderProcessingStarted(bSubagentId),
            OrderStdoutWritten(StdoutOutput),
            OrderProcessed(Succeeded(NamedValues.rc(0))),
            OrderMoved(Position(1) / "fork+🍋" % 2),
            OrderDetachable,
            OrderDetached)))

        // TORN EVENT STREAM
        val problem = controllerApi
          .eventAndStateObservable(fromEventId = Some(EventId.BeforeFirst))
          .take(1)
          .completedL
          .materialize
          .await(99.s)
          .failed.get.asInstanceOf[ProblemException]
          .problem
        assert(problem == SnapshotForUnknownEventIdProblem(EventId.BeforeFirst))

        val eventId = controllerApi.eventAndStateObservable(fromEventId = Some(finishedEventId))
          .headL.await(99.s).state.eventId
        assert(eventId == finishedEventId)
        controllerApi.stop.await(99.s)
      }
    }

  "Java history" in:
    runControllerAndBackup() { (primary, _, _, _, _, _, _) =>
      autoClosing(new JProxyContext) { context =>
        val api = context.newControllerApi(
          admissions.map(JAdmission.apply).toList.asJava,
          JHttpsConfig.empty)
        val javaTester = new JControllerApiHistoryTester(api, TestWorkflow.path, primary.agentEnvs.map(_.localUri).asJava)
        javaTester.test()
        javaTester.testTorn()
      }
    }


object ProxyHistoryTest:
  private val logger = Logger[this.type]
  private val AAgentPath = AgentPath("AGENT-A")
  private val BAgentPath = AgentPath("AGENT-B")
  private val aSubagentId = toLocalSubagentId(AAgentPath)
  private val bSubagentId = toLocalSubagentId(BAgentPath)
  private val TestWorkflow = WorkflowParser.parse(TestWorkflowId.asScala, s"""
     |define workflow {
     |  execute executable="${TestPathExecutable.path}", agent="AGENT-A";
     |  fork {
     |    "🥕": {
     |      execute executable="${TestPathExecutable.path}", agent="AGENT-A";
     |      execute executable="${TestPathExecutable.path}", agent="AGENT-A";
     |    },
     |    "🍋": {
     |      execute executable="${TestPathExecutable.path}", agent="AGENT-A";
     |      execute executable="${TestPathExecutable.path}", agent="AGENT-B";
     |    }
     |  }
     |  execute executable="${TestPathExecutable.path}", agent="AGENT-A";
     |}
     """.stripMargin.trim).orThrow

  private val TestOrder = FreshOrder(OrderId("🔺"), TestWorkflowId.path, Map("KEY" -> StringValue("VALUE")))
