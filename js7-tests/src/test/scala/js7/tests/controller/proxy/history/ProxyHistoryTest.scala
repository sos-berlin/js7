package js7.tests.controller.proxy.history

import com.softwaremill.diffx.scalatest.DiffMatcher._
import js7.base.auth.{UserAndPassword, UserId}
import js7.base.generic.SecretString
import js7.base.problem.Checked.Ops
import js7.base.problem.ProblemException
import js7.base.time.ScalaTime._
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.ScalaUtils._
import js7.common.akkautils.ProvideActorSystem
import js7.common.configutils.Configs.HoconStringInterpolator
import js7.common.scalautil.FileUtils.syntax.RichPath
import js7.common.scalautil.Logger
import js7.common.scalautil.MonixUtils.syntax._
import js7.common.time.WaitForCondition.waitForCondition
import js7.controller.data.ControllerCommand.TakeSnapshot
import js7.controller.data.ControllerState
import js7.controller.data.events.ControllerEvent.ControllerReady
import js7.data.Problems.SnapshotForUnknownEventIdProblem
import js7.data.agent.AgentId
import js7.data.event.{EventId, KeyedEvent, Stamped}
import js7.data.order.OrderEvent.{OrderAdded, OrderAttachable, OrderAttached, OrderDetachable, OrderDetached, OrderFinished, OrderForked, OrderJoined, OrderMoved, OrderProcessed, OrderProcessingStarted, OrderStarted, OrderStdoutWritten}
import js7.data.order.Outcome.{Succeeded, succeeded}
import js7.data.order.{FreshOrder, OrderEvent, OrderId}
import js7.data.value.{NamedValues, StringValue}
import js7.data.workflow.WorkflowParser
import js7.data.workflow.position.Position
import js7.journal.files.JournalFiles
import js7.proxy.configuration.ProxyConf
import js7.proxy.data.event.{EventAndState, ProxyStarted}
import js7.proxy.javaapi.JProxyContext
import js7.proxy.javaapi.data.auth.{JAdmission, JHttpsConfig}
import js7.proxy.{ControllerApi, JournaledProxy}
import js7.tests.controller.proxy.ClusterProxyTest
import js7.tests.controller.proxy.history.JControllerApiHistoryTester.TestWorkflowId
import js7.tests.controller.proxy.history.ProxyHistoryTest._
import js7.tests.testenv.ControllerClusterForScalaTest.TestExecutablePath
import js7.tests.testenv.DirectoryProvider.StdoutOutput
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalactic.source
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.language.implicitConversions

final class ProxyHistoryTest extends AnyFreeSpec with ProvideActorSystem with ClusterProxyTest
{
  override protected val versionedItems = TestWorkflow :: Nil
  override protected val agentIds = AAgentId :: BAgentId :: Nil
  private val controllerConfig = config"""
    js7.proxy.mirror.torn-older = 0s  # Should be irrelevant
    js7.journal.users-allowed-to-release-events = [ "Proxy" ]
    js7.journal.release-events-delay = 0s
    """
  override protected def primaryControllerConfig = controllerConfig withFallback super.primaryControllerConfig
  override protected def backupControllerConfig = controllerConfig withFallback super.backupControllerConfig

  "test" in {
    withControllerAndBackup() { (primary, backup, _) =>
      def listJournalFiles = JournalFiles.listJournalFiles(primary.controller.dataDir / "state" / "controller")
        .map(_.file.getFileName.toString)

      def assertJournalFileCount(n: Int)(implicit pos: source.Position): Unit = {
        waitForCondition(9.s, 10.ms) { listJournalFiles.size == n }
        assert(listJournalFiles.size == n)
      }

      val keyedEvents = mutable.Buffer[KeyedEvent[OrderEvent]]()

      runControllers(primary, backup) { (primaryController, _) =>
        val api = new ControllerApi(apiResources)
        api.executeCommand(TakeSnapshot).await(99.s).orThrow
        assertJournalFileCount(2)

        api.addOrder(TestOrder).await(99.s).orThrow
        val finishedEventId = primaryController.eventWatch.await[OrderFinished](_.key == TestOrder.id).head.eventId

        var releaseEventsEventId = EventId.BeforeFirst
        var lastAddedEventId = EventId.BeforeFirst
        primaryController.httpApi.login_(Some(UserAndPassword(UserId("TEST-USER"), SecretString("TEST-PASSWORD")))) await 99.s
        @volatile var lastState = ControllerState.empty
        @volatile var finished = false
        var rounds = 0
        while (!finished && rounds <= 100) {
          var proxyStartedReceived = false
          JournaledProxy.observable[ControllerState](apiResources, Some(lastState.eventId), _ => (), ProxyConf.default)
            .doOnNext(es => Task(scribe.debug(s"observe ${es.stampedEvent}")))
            .takeWhileInclusive {
              case EventAndState(Stamped(_, _, KeyedEvent(TestOrder.id, _: OrderFinished)), _, _) =>
                finished = true
                false
              case _=>
                true
            }
            .take(3)  // Process two events (and initial ProxyStarted) each test round
            .doOnNext(es => Task {
              es.stampedEvent.value.event match {
                case ProxyStarted =>
                  assert(!proxyStartedReceived)
                  proxyStartedReceived = true
                  es.state should matchTo(lastState)
                case _ =>
                  assert(lastState.eventId < es.stampedEvent.eventId)
              }
              lastState = es.state
              var keyedEvent = es.stampedEvent.value
              for (controllerReady <- ifCast[ControllerReady](keyedEvent.event)) {
                keyedEvent = keyedEvent.copy(event = controllerReady.copy(totalRunningTime = 333.s))
              }
              es.stampedEvent.value match {
                case KeyedEvent(orderId: OrderId, event: OrderEvent) => keyedEvents += orderId <-: event
                case _ =>
              }
            })
            .completedL
            .await(99.s)
          assert(proxyStartedReceived)
          rounds += 1
        }
        assert(rounds > 2)

        assertJournalFileCount(2)
        assert(listJournalFiles.contains("controller--0.journal"))
        api.releaseEvents(finishedEventId).await(99.s).orThrow
        assertJournalFileCount(1)
        assert(!listJournalFiles.contains("controller--0.journal"))  // First file deleted

        releaseEventsEventId = finishedEventId
        lastAddedEventId = primaryController.eventWatch.lastAddedEventId

        assert(keyedEvents.groupMap(_.key)(_.event).view.mapValues(_.toList).to(mutable.SortedMap) == mutable.SortedMap(
          OrderId("🔺") -> List(
            OrderAdded(TestWorkflowId.asScala, None, Map("KEY" -> StringValue("VALUE"))),
            OrderAttachable(AAgentId),
            OrderAttached(AAgentId),
            OrderStarted,
            OrderProcessingStarted,
            OrderStdoutWritten(StdoutOutput),
            OrderProcessed(Succeeded(NamedValues.rc(0))),
            OrderMoved(Position(1)),
            OrderForked(List(
              OrderForked.Child("🥕",OrderId("🔺|🥕")),
              OrderForked.Child("🍋",OrderId("🔺|🍋")))),
            OrderDetachable,
            OrderDetached,
            OrderJoined(succeeded),
            OrderMoved(Position(2)),
            OrderAttachable(AAgentId),
            OrderAttached(AAgentId),
            OrderProcessingStarted,
            OrderStdoutWritten(StdoutOutput),
            OrderProcessed(Succeeded(NamedValues.rc(0))),
            OrderMoved(Position(3)),
            OrderDetachable,
            OrderDetached,
            OrderFinished),
          OrderId("🔺|🥕") -> List(
            OrderProcessingStarted,
            OrderStdoutWritten(StdoutOutput),
            OrderProcessed(Succeeded(NamedValues.rc(0))),
            OrderMoved(Position(1) / "fork+🥕" % 1),
            OrderProcessingStarted,
            OrderStdoutWritten(StdoutOutput),
            OrderProcessed(Succeeded(NamedValues.rc(0))),
            OrderMoved(Position(1) / "fork+🥕" % 2),
            OrderDetachable,
            OrderDetached),
        OrderId("🔺|🍋") -> List(
          OrderProcessingStarted,
          OrderStdoutWritten(StdoutOutput),
          OrderProcessed(Succeeded(NamedValues.rc(0))),
          OrderMoved(Position(1) / "fork+🍋" % 1),
          OrderDetachable,
          OrderDetached,
          OrderAttachable(BAgentId),
          OrderAttached(BAgentId),
          OrderProcessingStarted,
          OrderStdoutWritten(StdoutOutput),
          OrderProcessed(Succeeded(NamedValues.rc(0))),
          OrderMoved(Position(1) / "fork+🍋" % 2),
          OrderDetachable,
          OrderDetached)))

        // TORN EVENT STREAM
        val problem = JournaledProxy.observable[ControllerState](apiResources, fromEventId = Some(EventId.BeforeFirst), _ => (), ProxyConf.default)
          .take(1).completedL.materialize.await(99.s).failed.get.asInstanceOf[ProblemException].problem
        assert(problem == SnapshotForUnknownEventIdProblem(EventId.BeforeFirst))

        val eventId = JournaledProxy.observable[ControllerState](apiResources, fromEventId = Some(finishedEventId), _ => (), ProxyConf.default)
          .headL.await(99.s).state.eventId
        assert(eventId == finishedEventId)
      }
    }
  }

  "Java history" in {
    runControllerAndBackup() { (primary, _, _, _, _) =>
      autoClosing(new JProxyContext) { context =>
        val api = context.newControllerApi(admissions.map(JAdmission.apply).asJava, JHttpsConfig.empty)
        val javaTester = new JControllerApiHistoryTester(api, TestWorkflow.path, primary.agents.map(_.localUri).asJava)
        javaTester.test()
        javaTester.testTorn()
      }
    }
  }
}

object ProxyHistoryTest
{
  private val logger = Logger(getClass)
  private val AAgentId = AgentId("AGENT-A")
  private val BAgentId = AgentId("AGENT-B")
  private val TestWorkflow = WorkflowParser.parse(TestWorkflowId.asScala, s"""
     |define workflow {
     |  execute executable="${TestExecutablePath.path}", agent="AGENT-A";
     |  fork {
     |    "🥕": {
     |      execute executable="${TestExecutablePath.path}", agent="AGENT-A";
     |      execute executable="${TestExecutablePath.path}", agent="AGENT-A";
     |    },
     |    "🍋": {
     |      execute executable="${TestExecutablePath.path}", agent="AGENT-A";
     |      execute executable="${TestExecutablePath.path}", agent="AGENT-B";
     |    }
     |  }
     |  execute executable="${TestExecutablePath.path}", agent="AGENT-A";
     |}
     """.stripMargin.trim).orThrow

  private val TestOrder = FreshOrder(OrderId("🔺"), TestWorkflowId.path, arguments = Map("KEY" -> StringValue("VALUE")))
}
