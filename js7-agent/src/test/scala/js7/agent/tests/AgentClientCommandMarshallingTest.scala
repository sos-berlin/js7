package js7.agent.tests

import com.google.inject.{AbstractModule, Provides}
import javax.inject.Singleton
import js7.agent.client.SimpleAgentClient
import js7.agent.command.CommandHandler
import js7.agent.data.commands.AgentCommand
import js7.agent.data.commands.AgentCommand.{EmergencyStop, ShutDown}
import js7.agent.tests.AgentClientCommandMarshallingTest.*
import js7.base.io.process.ProcessSignal.SIGTERM
import js7.base.problem.Checked
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.Closer.syntax.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.SideEffect.ImplicitSideEffect
import js7.core.command.CommandMeta
import monix.eval.Task
import monix.execution.Scheduler.Implicits.traced
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.freespec.AnyFreeSpec

/**
 * @author Joacim Zschimmer
 */
final class AgentClientCommandMarshallingTest
extends AnyFreeSpec with ScalaFutures with AgentTester {

  override protected def extraAgentModule = new AbstractModule {
    @Provides @Singleton
    def commandHandler(): CommandHandler = new CommandHandler {
      def execute(command: AgentCommand, meta: CommandMeta): Task[Checked[command.Response]] =
        Task {
          (command match {
            case ExpectedTerminate => Right(AgentCommand.Response.Accepted)
            case EmergencyStop(false) => Right(AgentCommand.Response.Accepted)
            case _ => throw new NotImplementedError
          })
          .map(_.asInstanceOf[command.Response])
        }

      def overview = throw new NotImplementedError
      def detailed = throw new NotImplementedError
    }
  }
  override implicit val patienceConfig = PatienceConfig(timeout = 10.s)
  private lazy val client = new SimpleAgentClient(agent.localUri, None).closeWithCloser
    .sideEffect(_.setSessionToken(agent.sessionToken))

  List[(AgentCommand, Checked[AgentCommand.Response])](
    ExpectedTerminate -> Right(AgentCommand.Response.Accepted),
    EmergencyStop(false) -> Right(AgentCommand.Response.Accepted))
  .foreach { case (command, response) =>
    command.getClass.simpleScalaName in {
      assert(client.commandExecute(command).await(99.s) == response)
    }
  }
}

private object AgentClientCommandMarshallingTest {
  private val ExpectedTerminate = ShutDown(Some(SIGTERM))
}
