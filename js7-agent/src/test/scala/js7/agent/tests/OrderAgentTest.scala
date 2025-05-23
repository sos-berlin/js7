package js7.agent.tests

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import js7.agent.TestAgent
import js7.agent.client.AgentClient
import js7.agent.configuration.AgentConfiguration
import js7.agent.data.AgentState
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.{AttachItem, AttachOrder, AttachSignedItem, Batch, DedicateAgentDirector, DetachOrder}
import js7.agent.tests.OrderAgentTest.*
import js7.agent.tests.TestAgentDirectoryProvider.{TestUserAndPassword, provideAgentDirectory}
import js7.base.Problems.TamperedWithSignedMessageProblem
import js7.base.auth.Admission
import js7.base.configutils.Configs.*
import js7.base.crypt.Signed
import js7.base.io.file.FileUtils.syntax.*
import js7.base.log.{CorrelId, CorrelIdWrapped}
import js7.base.monixlike.MonixLikeExtensions.{headL, toListL}
import js7.base.problem.Checked.Ops
import js7.base.problem.{Checked, Problem}
import js7.base.system.OperatingSystem.isWindows
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch
import js7.base.utils.Closer.syntax.*
import js7.base.utils.Closer.withCloser
import js7.base.web.Uri
import js7.common.pekkoutils.Pekkos
import js7.common.pekkoutils.Pekkos.newActorSystem
import js7.data.agent.{AgentPath, AgentRef}
import js7.data.controller.{ControllerId, ControllerRunId}
import js7.data.event.{Event, EventRequest, JournalId, KeyedEvent, Stamped}
import js7.data.item.{ItemSigner, SignableItem}
import js7.data.order.OrderEvent.OrderDetachable
import js7.data.order.{HistoricOutcome, Order, OrderId, OrderOutcome}
import js7.data.subagent.{SubagentId, SubagentItem}
import js7.data.value.{NumberValue, StringValue}
import js7.data.workflow.position.Position
import js7.data.workflow.test.TestSetting.*
import js7.service.pgp.PgpSigner
import org.apache.pekko.actor.ActorSystem
import org.scalatest.matchers.should.Matchers.*
import scala.collection.mutable
import scala.concurrent.duration.*

/**
  * @author Joacim Zschimmer
  */
final class OrderAgentTest extends OurTestSuite:

  private given IORuntime = ioRuntime

  private lazy val controllerRunId = ControllerRunId(JournalId.random())

  "AgentCommand AttachOrder" in:
    provideAgentDirectory { directory =>
      directory / "config" / "private" / "trusted-pgp-keys" / "test.asc" := verifier.publicKeys.head
      directory / "config" / "private" / "private.conf" ++=
        s"""js7.configuration.trusted-signature-keys.PGP = $${js7.config-directory}"/private/trusted-pgp-keys"
           |""".stripMargin

      val jobDir = directory / "config" / "executables"
      APathExecutable.toFile(jobDir).writeUtf8Executable(TestScript)
      BPathExecutable.toFile(jobDir).writeUtf8Executable(TestScript)

      val agentConf = AgentConfiguration.forTest(directory, name = "OrderAgentTest")
      TestAgent.blockingRun(agentConf, 99.s) { agent =>
        withCloser { implicit closer =>
          import agent.actorSystem
          val agentClient = AgentClient(Admission(agent.localUri, Some(TestUserAndPassword)))
            .closeWithCloser

          assert(agentClient
            .commandExecute(
              DedicateAgentDirector(Seq(subagentId), controllerId, controllerRunId, agentPath))
            .await(99.s) ==
            Left(Problem(s"HTTP 401 Unauthorized: POST ${agent.localUri}/agent/api/command => " +
              "The resource requires authentication, which was not supplied with the request")))
          agentClient.login().await(99.s)

          // Without Login, this registers all anonymous clients
          assert(agentClient
            .commandExecute(
              DedicateAgentDirector(Seq(subagentId), controllerId, controllerRunId, agentPath))
            .await(99.s).orThrow.isInstanceOf[DedicateAgentDirector.Response])

          agentClient
            .commandExecute(
              AttachItem(AgentRef(agentPath, directors = Seq(subagentId))))
            .await(99.s).orThrow

          agentClient
            .commandExecute(
              AttachItem(SubagentItem(subagentId, agentPath, Uri("http://127.0.0.1:0"))))
            .await(99.s).orThrow

          val order = Order(OrderId("TEST-ORDER"), SimpleTestWorkflow.id /: Position(0),
            Order.Ready(), Map(
              "x" -> StringValue("X")))

          def attachOrder(signedWorkflow: Signed[SignableItem])
          : Checked[AgentCommand.Response.Accepted] =
            for
              _ <- agentClient.commandExecute(AttachSignedItem(signedWorkflow)).await(99.s)
              x <- agentClient.commandExecute(AttachOrder(order, TestAgentPath)).await(99.s)
            yield x

          val tamperedWorkflow = signedSimpleWorkflow.copy(
            signedString = signedSimpleWorkflow.signedString.copy(
              string = signedSimpleWorkflow.signedString.string + " "))
          attachOrder(tamperedWorkflow) shouldEqual Left(TamperedWithSignedMessageProblem)

          attachOrder(signedSimpleWorkflow) shouldEqual Right(AgentCommand.Response.Accepted)

          // Await OrderDetachable
          agentClient
            .agentEventStream(EventRequest.singleClass[Event](timeout = Some(10.s)))
            .flatMap(_
              .orThrow
              .collectFirst:
                case Stamped(_, _, KeyedEvent(order.id, OrderDetachable)) =>
              .headL)
            .await(99.s)

          val processedOrder = agent.currentAgentState().idToOrder(order.id)
          assert(processedOrder == toExpectedOrder(order))
          assert(agentClient.commandExecute(DetachOrder(order.id)).await(99.s) ==
            Right(AgentCommand.Response.Accepted))

          //TODO assert((agentClient.task.overview await 99.s) == TaskRegisterOverview(currentTaskCount = 0, totalTaskCount = 1))
        }
      }
    }

  for testSpeed <- sys.props.get("test.speed") do s"Speed test $testSpeed orders × ${/*SimpleTestWorkflow.jobNodeCount*/"·"} jobs" in:
    // TODO Speed test does not work
    val n = testSpeed.toInt
    provideAgentDirectory { directory =>
      val executableDir = directory / "config" / "executables"
      APathExecutable.toFile(executableDir).writeUtf8Executable(TestScript)
      BPathExecutable.toFile(executableDir).writeUtf8Executable(TestScript)
      val agentConf = AgentConfiguration.forTest(
        directory,
        name = "OrderAgentTest",
        config"""
          js7.journal.sync = on
          js7.journal.delay = 0ms
          js7.journal.simulate-sync = 10ms
          js7.journal.snapshot.log-period = 1ms
          js7.journal.snapshot.log-actor-limit = 10
          """)
      val timeout = 1.hour
      TestAgent.blockingRun(agentConf, timeout) { agent =>
        withCloser { implicit closer =>
          implicit val actorSystem: ActorSystem =
            newActorSystem(getClass.getSimpleName, executionContext = ioRuntime.compute)
          val agentClient = AgentClient(Admission(agent.localUri, Some(TestUserAndPassword)))
            .closeWithCloser
          agentClient.login().await(99.s)
          assert(
            agentClient
              .commandExecute(
                DedicateAgentDirector(Seq(SubagentId("SUBAGENT")), controllerId, controllerRunId, agentPath))
              .await(99.s).isRight)

          val orders = for i <- 1 to n yield
            Order(OrderId(s"TEST-ORDER-$i"), SimpleTestWorkflow.id /: Position(0), Order.Ready(),
              Map("x" -> StringValue("X")),
              attachedState = Some(Order.Attached(AgentPath("AGENT"))))

          val stopwatch = new Stopwatch
          agentClient.commandExecute(Batch(
            Seq(AttachSignedItem(signedSimpleWorkflow))
              .concat(orders.map(AttachOrder(_)))
              .map(CorrelIdWrapped(CorrelId.empty, _)))
          ).await(99.s)

          val awaitedOrderIds = orders.map(_.id).toSet
          val ready = mutable.Set.empty[OrderId]
          while ready != awaitedOrderIds do
            ready ++= agentClient
              .agentEventStream(EventRequest.singleClass[Event](timeout = Some(timeout)))
              .map(_.orThrow)
              .await(99.s)
              .map(_.value)
              .collect { case KeyedEvent(orderId: OrderId, OrderDetachable) => orderId }
              .toListL
              .await(99.s)
          agentClient.commandExecute(Batch(orders
            .map(o => DetachOrder(o.id))
            .map(CorrelIdWrapped(CorrelId.empty, _))))
            .await(99.s).orThrow
          info(stopwatch.itemsPerSecondString(n, "orders"))

          agentClient.commandExecute(AgentCommand.ShutDown()).await(99.s).orThrow
          Pekkos.terminateAndWait(actorSystem)
        }
      }
    }

private object OrderAgentTest:
  private val agentPath = AgentPath("AGENT")
  private val subagentId = SubagentId("SUBAGENT")
  private val controllerId = ControllerId("CONTROLLER")
  private val TestScript =
    if isWindows then """
      |@echo off
      |echo result=TEST-RESULT-%SCHEDULER_PARAM_JOB_B% >>"%SCHEDULER_RETURN_VALUES%"
      |""".stripMargin
    else """
      |echo "result=TEST-RESULT-$SCHEDULER_PARAM_JOB_B" >>"$SCHEDULER_RETURN_VALUES"
      |""".stripMargin

  private val (signer, verifier) = PgpSigner.forTest()
  private val itemSigner = new ItemSigner(signer, AgentState.versionedItemJsonCodec)
  private val signedSimpleWorkflow = itemSigner.sign(SimpleTestWorkflow)

  private def toExpectedOrder(order: Order[Order.State]) =
    order.copy(
      workflowPosition = order.workflowPosition.copy(position = Position(2)),
      attachedState = Some(Order.Detaching(TestAgentPath)),
      arguments = Map("x" -> StringValue("X")),
      historicOutcomes = Vector(
        HistoricOutcome(Position(0), OrderOutcome.Succeeded(Map(
          "returnCode" -> NumberValue(0),
          "result" -> StringValue("TEST-RESULT-")))),
        HistoricOutcome(Position(1), OrderOutcome.Succeeded(Map(
          "returnCode" -> NumberValue(0),
          "result" -> StringValue("TEST-RESULT-B-VALUE"))))))
