package js7.agent.tests

import js7.agent.client.SimpleAgentClient
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.{EmergencyStop, ShutDown}
import js7.agent.tests.AgentClientCommandMarshallingTest.*
import js7.base.io.process.ProcessSignal.SIGTERM
import js7.base.problem.Checked
import js7.base.test.OurTestSuite
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.Closer.syntax.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.SideEffect.ImplicitSideEffect
import monix.execution.Scheduler.Implicits.traced
import org.scalatest.concurrent.ScalaFutures

/**
 * @author Joacim Zschimmer
 */
final class AgentClientCommandMarshallingTest
extends OurTestSuite with ScalaFutures with AgentTester:

  //override protected val agentTestWiring = RunningAgent.TestWiring(
  //  commandHandler = Some(new CommandHandler {
  //    def execute(command: AgentCommand, meta: CommandMeta): Task[Checked[command.Response]] =
  //      Task {
  //        (command match {
  //          case ExpectedTerminate => Right(AgentCommand.Response.Accepted)
  //          case EmergencyStop(false) => Right(AgentCommand.Response.Accepted)
  //          case _ => throw new NotImplementedError
  //        })
  //        .map(_.asInstanceOf[command.Response])
  //      }
  //  }))

  override implicit val patienceConfig = PatienceConfig(timeout = 10.s)
  private lazy val client = new SimpleAgentClient(agent.localUri, None).closeWithCloser
    .sideEffect(_.setSessionToken(agent.sessionToken))

  List[(AgentCommand, Checked[AgentCommand.Response])](
    ExpectedTerminate -> Right(AgentCommand.Response.Accepted),
    EmergencyStop(false) -> Right(AgentCommand.Response.Accepted))
  .foreach { case (command, response) =>
    command.getClass.simpleScalaName in:
      pending
      assert(client.commandExecute(command).await(99.s) == response)
  }

private object AgentClientCommandMarshallingTest:
  private val ExpectedTerminate = ShutDown(Some(SIGTERM))
