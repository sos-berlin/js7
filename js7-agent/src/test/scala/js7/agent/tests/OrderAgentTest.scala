package js7.agent.tests

import js7.agent.RunningAgent
import js7.agent.client.AgentClient
import js7.agent.configuration.AgentConfiguration
import js7.agent.configuration.Akkas.newAgentActorSystem
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.{AttachOrder, Batch, DetachOrder, RegisterAsController}
import js7.agent.tests.OrderAgentTest._
import js7.agent.tests.TestAgentDirectoryProvider.{TestUserAndPassword, provideAgentDirectory}
import js7.base.Problems.TamperedWithSignedMessageProblem
import js7.base.crypt.SignedString
import js7.base.problem.Checked.Ops
import js7.base.problem.{Checked, Problem}
import js7.base.system.OperatingSystem.isWindows
import js7.base.time.ScalaTime._
import js7.base.time.Stopwatch
import js7.base.utils.Closer.syntax._
import js7.base.utils.Closer.withCloser
import js7.common.configutils.Configs._
import js7.common.crypt.pgp.PgpSigner
import js7.common.scalautil.FileUtils.syntax._
import js7.common.scalautil.MonixUtils.syntax._
import js7.data.agent.AgentId
import js7.data.event.{Event, EventRequest, EventSeq, KeyedEvent, Stamped}
import js7.data.item.VersionedItemSigner
import js7.data.order.OrderEvent.OrderDetachable
import js7.data.order.{HistoricOutcome, Order, OrderId, Outcome}
import js7.data.value.{NumericValue, StringValue}
import js7.data.workflow.Workflow
import js7.data.workflow.position.Position
import js7.data.workflow.test.TestSetting._
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._
import scala.collection.mutable
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class OrderAgentTest extends AnyFreeSpec
{
  "AgentCommand AttachOrder" in {
    provideAgentDirectory { directory =>
      directory / "config" / "private" / "trusted-pgp-keys" / "test.asc" := verifier.publicKeys.head
      directory / "config" / "private" / "private.conf" ++=
        s"""js7.configuration.trusted-signature-keys.PGP = $${js7.config-directory}"/private/trusted-pgp-keys"
           |""".stripMargin

      val jobDir = directory / "config" / "executables"
      AExecutablePath.toFile(jobDir).writeExecutable(TestScript)
      BExecutablePath.toFile(jobDir).writeExecutable(TestScript)

      val agentConf = AgentConfiguration.forTest(directory)
      RunningAgent.run(agentConf, timeout = Some(99.s)) { agent =>
        withCloser { implicit closer =>
          implicit val actorSystem = newAgentActorSystem(getClass.getSimpleName)
          val agentClient = AgentClient(agent.localUri, Some(TestUserAndPassword)).closeWithCloser
          assert(agentClient.commandExecute(RegisterAsController(agentId)).await(99.s) ==
            Left(Problem(s"HTTP 401 Unauthorized: POST ${agent.localUri}/agent/api/command => " +
              "The resource requires authentication, which was not supplied with the request")))
          agentClient.login() await 99.s
          assert(agentClient.commandExecute(RegisterAsController(agentId)).await(99.s).toOption.get  // Without Login, this registers all anonymous clients
            .isInstanceOf[RegisterAsController.Response])

          val order = Order(OrderId("TEST-ORDER"), SimpleTestWorkflow.id, Order.Ready, Map("x" -> StringValue("X")))

          def attachOrder(signedWorkflow: SignedString): Checked[AgentCommand.Response.Accepted] =
            agentClient.commandExecute(AttachOrder(order, TestAgentId, signedWorkflow)).await(99.s)

          attachOrder(SignedSimpleWorkflow.copy(string = SignedSimpleWorkflow.string + " ")) shouldEqual Left(TamperedWithSignedMessageProblem)

          attachOrder(SignedSimpleWorkflow) shouldEqual Right(AgentCommand.Response.Accepted)

          EventRequest.singleClass[Event](timeout = Some(10.s))
            .repeat(eventRequest => agentClient.controllersEvents(eventRequest).map(_.orThrow).runToFuture) {
              case Stamped(_, _, KeyedEvent(order.id, OrderDetachable)) =>
            }
          val Right(processedOrder) = agentClient.order(order.id) await 99.s
          assert(processedOrder == toExpectedOrder(order))
          agentClient.commandExecute(DetachOrder(order.id)) await 99.s shouldEqual Right(AgentCommand.Response.Accepted)
          //TODO assert((agentClient.task.overview await 99.s) == TaskRegisterOverview(currentTaskCount = 0, totalTaskCount = 1))

          try agentClient.commandExecute(AgentCommand.ShutDown()).await(99.s).orThrow
          catch { case t: akka.stream.StreamTcpException if t.getMessage contains "Connection reset by peer" => }
        }
      }
    }
  }

  for (testSpeed <- sys.props.get("test.speed")) s"Speed test $testSpeed orders × ${/*SimpleTestWorkflow.jobNodeCount*/"·"} jobs" in {
    val n = testSpeed.toInt
    provideAgentDirectory { directory =>
      val executableDir = directory / "config" / "executables"
      AExecutablePath.toFile(executableDir).writeExecutable(TestScript)
      BExecutablePath.toFile(executableDir).writeExecutable(TestScript)
      val agentConf = AgentConfiguration.forTest(directory, config"""
        js7.journal.sync = on
        js7.journal.delay = 0ms
        js7.journal.simulate-sync = 10ms
        js7.journal.snapshot.log-period = 1ms
        js7.journal.snapshot.log-actor-limit = 10
        """)
      val timeout = 1.hour
      RunningAgent.run(agentConf, timeout = Some(timeout)) { agent =>
        withCloser { implicit closer =>
          implicit val actorSystem = newAgentActorSystem(getClass.getSimpleName)
          val agentClient = AgentClient(agent.localUri, Some(TestUserAndPassword)).closeWithCloser
          agentClient.login() await 99.s
          assert(agentClient.commandExecute(RegisterAsController(agentId)).await(99.s) == Right(AgentCommand.Response.Accepted))

          val orders = for (i <- 1 to n) yield
            Order(OrderId(s"TEST-ORDER-$i"), SimpleTestWorkflow.id, Order.Ready,
              Map("x" -> StringValue("X")),
              attachedState = Some(Order.Attached(AgentId("AGENT"))))

          val stopwatch = new Stopwatch
          agentClient.commandExecute(Batch(orders.map(AttachOrder(_, SignedSimpleWorkflow)))) await 99.s

          val awaitedOrderIds = orders.map(_.id).toSet
          val ready = mutable.Set[OrderId]()
          while (
            agentClient.controllersEvents(EventRequest.singleClass[Event](timeout = Some(timeout))).map(_.orThrow) await 99.s match {
              case EventSeq.NonEmpty(stampeds) =>
                ready ++= stampeds.map(_.value) collect { case KeyedEvent(orderId: OrderId, OrderDetachable) => orderId }
                ready != awaitedOrderIds
              case _ =>
                true
            }
          ) {}
          agentClient.commandExecute(Batch(orders map { o => DetachOrder(o.id) })).await(99.s).orThrow
          info(stopwatch.itemsPerSecondString(n, "orders"))

          agentClient.commandExecute(AgentCommand.ShutDown()).await(99.s).orThrow
        }
      }
    }
  }
}

private object OrderAgentTest
{
  private val agentId = AgentId("AGENT")
  private val TestScript =
    if (isWindows) """
      |@echo off
      |echo result=TEST-RESULT-%SCHEDULER_PARAM_JOB_B% >>"%SCHEDULER_RETURN_VALUES%"
      |""".stripMargin
    else """
      |echo "result=TEST-RESULT-$SCHEDULER_PARAM_JOB_B" >>"$SCHEDULER_RETURN_VALUES"
      |""".stripMargin

  private val (signer, verifier) = PgpSigner.forTest()
  private val itemSigner = new VersionedItemSigner(signer, Workflow.jsonEncoder)
  private val SignedSimpleWorkflow = itemSigner.sign(SimpleTestWorkflow)

  private def toExpectedOrder(order: Order[Order.State]) =
    order.copy(
      workflowPosition = order.workflowPosition.copy(position = Position(2)),
      attachedState = Some(Order.Detaching(TestAgentId)),
      arguments = Map("x" -> StringValue("X")),
      historicOutcomes =
        HistoricOutcome(Position(0), Outcome.Succeeded(Map("returnCode" -> NumericValue(0), "result" -> StringValue("TEST-RESULT-")))) ::
        HistoricOutcome(Position(1), Outcome.Succeeded(Map("returnCode" -> NumericValue(0), "result" -> StringValue("TEST-RESULT-B-VALUE")))) :: Nil)
}
