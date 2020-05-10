package com.sos.jobscheduler.agent.tests

import akka.http.scaladsl.model.StatusCodes.Unauthorized
import com.sos.jobscheduler.agent.RunningAgent
import com.sos.jobscheduler.agent.client.AgentClient
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.configuration.Akkas.newAgentActorSystem
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.data.commands.AgentCommand.{AttachOrder, Batch, DetachOrder, RegisterAsMaster}
import com.sos.jobscheduler.agent.tests.OrderAgentTest._
import com.sos.jobscheduler.agent.tests.TestAgentDirectoryProvider.{TestUserAndPassword, provideAgentDirectory}
import com.sos.jobscheduler.base.Problems.TamperedWithSignedMessageProblem
import com.sos.jobscheduler.base.crypt.SignedString
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.problem.Checked.Ops
import com.sos.jobscheduler.base.time.ScalaTime._
import com.sos.jobscheduler.base.time.Stopwatch
import com.sos.jobscheduler.base.utils.Closer.syntax._
import com.sos.jobscheduler.base.utils.Closer.withCloser
import com.sos.jobscheduler.common.http.AkkaHttpClient
import com.sos.jobscheduler.common.scalautil.FileUtils.syntax._
import com.sos.jobscheduler.common.scalautil.MonixUtils.syntax._
import com.sos.jobscheduler.common.system.OperatingSystem.isWindows
import com.sos.jobscheduler.core.crypt.pgp.PgpSigner
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.event.{Event, EventRequest, EventSeq, KeyedEvent, Stamped}
import com.sos.jobscheduler.data.filebased.FileBasedSigner
import com.sos.jobscheduler.data.order.OrderEvent.OrderDetachable
import com.sos.jobscheduler.data.order.{HistoricOutcome, Order, OrderId, Outcome}
import com.sos.jobscheduler.data.workflow.Workflow
import com.sos.jobscheduler.data.workflow.position.Position
import com.sos.jobscheduler.data.workflow.test.TestSetting._
import com.typesafe.config.ConfigFactory
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers._
import scala.collection.mutable
import scala.concurrent.duration._

/**
  * @author Joacim Zschimmer
  */
final class OrderAgentTest extends AnyFreeSpec {

  "AgentCommand AttachOrder" in {
    provideAgentDirectory { directory =>
      directory / "config" / "private" / "trusted-pgp-keys" / "test.asc" := signer.publicKey
      directory / "config" / "private" / "private.conf" ++=
        s"""|jobscheduler.configuration.trusted-signature-keys.PGP = $${jobscheduler.config-directory}"/private/trusted-pgp-keys"
           |""".stripMargin

      val jobDir = directory / "config" / "executables"
      AExecutablePath.toFile(jobDir).writeExecutable(TestScript)
      BExecutablePath.toFile(jobDir).writeExecutable(TestScript)

      val agentConf = AgentConfiguration.forTest(directory)
      RunningAgent.run(agentConf, timeout = Some(99.s)) { agent =>
        withCloser { implicit closer =>
          implicit val actorSystem = newAgentActorSystem(getClass.getSimpleName)
          val agentClient = AgentClient(agent.localUri, Some(TestUserAndPassword)).closeWithCloser
          intercept[AkkaHttpClient.HttpException] {  // Login is required
            agentClient.commandExecute(RegisterAsMaster).await(99.s).orThrow
          } .status shouldEqual Unauthorized
          agentClient.login() await 99.s
          assert(agentClient.commandExecute(RegisterAsMaster).await(99.s).toOption.get  // Without Login, this registers all anonymous clients
            .isInstanceOf[RegisterAsMaster.Response])

          val order = Order(OrderId("TEST-ORDER"), SimpleTestWorkflow.id, Order.Ready, Map("x" -> "X"))

          def attachOrder(signedWorkflow: SignedString): Checked[AgentCommand.Response.Accepted] =
            agentClient.commandExecute(AttachOrder(order, TestAgentRefPath, signedWorkflow)).await(99.s)

          attachOrder(SignedSimpleWorkflow.copy(string = SignedSimpleWorkflow.string + " ")) shouldEqual Left(TamperedWithSignedMessageProblem)

          attachOrder(SignedSimpleWorkflow) shouldEqual Right(AgentCommand.Response.Accepted)

          EventRequest.singleClass[Event](timeout = Some(10.s))
            .repeat(eventRequest => agentClient.mastersEvents(eventRequest).map(_.orThrow).runToFuture) {
              case Stamped(_, _, KeyedEvent(order.id, OrderDetachable)) =>
            }
          val Right(processedOrder) = agentClient.order(order.id) await 99.s
          assert(processedOrder == toExpectedOrder(order))
          agentClient.commandExecute(DetachOrder(order.id)) await 99.s shouldEqual Right(AgentCommand.Response.Accepted)
          //TODO assert((agentClient.task.overview await 99.s) == TaskRegisterOverview(currentTaskCount = 0, totalTaskCount = 1))
          agentClient.commandExecute(AgentCommand.ShutDown()).await(99.s).orThrow
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
      val agentConf = AgentConfiguration.forTest(directory, ConfigFactory.parseString("""
         |jobscheduler.journal.sync = on
         |jobscheduler.journal.delay = 0ms
         |jobscheduler.journal.simulate-sync = 10ms
         |jobscheduler.journal.snapshot.log-period = 1ms
         |jobscheduler.journal.snapshot.log-actor-limit = 10
         |""".stripMargin))
      val timeout = 1.hour
      RunningAgent.run(agentConf, timeout = Some(timeout)) { agent =>
        withCloser { implicit closer =>
          implicit val actorSystem = newAgentActorSystem(getClass.getSimpleName)
          val agentClient = AgentClient(agent.localUri, Some(TestUserAndPassword)).closeWithCloser
          agentClient.login() await 99.s
          assert(agentClient.commandExecute(RegisterAsMaster).await(99.s) == Right(AgentCommand.Response.Accepted))

          val orders = for (i <- 1 to n) yield
            Order(OrderId(s"TEST-ORDER-$i"), SimpleTestWorkflow.id, Order.Ready,
              Map("x" -> "X"),
              attachedState = Some(Order.Attached(AgentRefPath("/AGENT"))))

          val stopwatch = new Stopwatch
          agentClient.commandExecute(Batch(orders map { AttachOrder(_, SignedSimpleWorkflow) })) await 99.s

          val awaitedOrderIds = (orders map { _.id }).toSet
          val ready = mutable.Set[OrderId]()
          while (
            agentClient.mastersEvents(EventRequest.singleClass[Event](timeout = Some(timeout))).map(_.orThrow) await 99.s match {
              case EventSeq.NonEmpty(stampeds) =>
                ready ++= stampeds map { _.value } collect { case KeyedEvent(orderId: OrderId, OrderDetachable) => orderId }
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

private object OrderAgentTest {
  private val TestScript =
    if (isWindows) """
      |@echo off
      |echo result=TEST-RESULT-%SCHEDULER_PARAM_JOB_B% >>"%SCHEDULER_RETURN_VALUES%"
      |""".stripMargin
    else """
      |echo "result=TEST-RESULT-$SCHEDULER_PARAM_JOB_B" >>"$SCHEDULER_RETURN_VALUES"
      |""".stripMargin

  private val signer = PgpSigner.forTest
  private val fileBasedSigner = new FileBasedSigner(signer, Workflow.jsonEncoder)
  private val SignedSimpleWorkflow = fileBasedSigner.sign(SimpleTestWorkflow)

  private def toExpectedOrder(order: Order[Order.State]) =
    order.copy(
      workflowPosition = order.workflowPosition.copy(position = Position(2)),
      attachedState = Some(Order.Detaching(TestAgentRefPath)),
      arguments = Map("x" -> "X"),
      historicOutcomes =
        HistoricOutcome(Position(0), Outcome.Succeeded(Map("result" -> "TEST-RESULT-"))) ::
        HistoricOutcome(Position(1), Outcome.Succeeded(Map("result" -> "TEST-RESULT-B-VALUE"))) :: Nil)
}
