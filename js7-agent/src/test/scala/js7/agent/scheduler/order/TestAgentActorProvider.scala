package js7.agent.scheduler.order

import akka.actor.{ActorRef, ActorSystem, Props}
import com.google.inject.Guice
import java.nio.file.Path
import js7.agent.configuration.AgentConfiguration
import js7.agent.configuration.Akkas.newAgentActorSystem
import js7.agent.configuration.inject.AgentModule
import js7.agent.data.commands.AgentCommand
import js7.agent.scheduler.AgentActor
import js7.agent.scheduler.order.TestAgentActorProvider._
import js7.agent.tests.TestAgentDirectoryProvider
import js7.base.auth.UserId
import js7.base.problem.Checked
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.Closer.syntax.RichClosersAutoCloseable
import js7.base.utils.{Closer, HasCloser, ProgramTermination}
import js7.common.guice.GuiceImplicits.RichInjector
import scala.concurrent.{Future, Promise}

/**
  * @author Joacim Zschimmer
  */
private class TestAgentActorProvider(testName: String) extends HasCloser
{
  private val directoryProvider = TestAgentDirectoryProvider().closeWithCloser
  lazy val agentDirectory = directoryProvider.agentDirectory

  lazy val agentActor = start(agentDirectory, testName)

  def startAgent() = agentActor

  def itemSigner = directoryProvider.itemSigner

  def executeCommand(command: AgentCommand): Future[Checked[AgentCommand.Response]] = {
    val response = Promise[Checked[AgentCommand.Response]]()
    agentActor ! AgentActor.Input.ExternalCommand(ControllerUserId, command, response)
    response.future
  }
}

object TestAgentActorProvider {
  private val ControllerUserId = UserId.Anonymous

  def provide[A](testName: String)(body: TestAgentActorProvider => A): A =
    autoClosing(new TestAgentActorProvider(testName))(body)

  private def start(configAndData: Path, testName: String)(implicit closer: Closer): ActorRef = {
    implicit val agentConfiguration = AgentConfiguration.forTest(configAndData = configAndData, name = testName)
    val actorSystem = newAgentActorSystem(name = testName)
    val injector = Guice.createInjector(new AgentModule(agentConfiguration))

    // Initialize Akka here to solve a classloader problem when Akka reads its reference.conf
    injector.instance[ActorSystem]

    val terminatePromise = Promise[ProgramTermination]()
    val agentActor = actorSystem.actorOf(Props { injector.instance[AgentActor.Factory].apply(terminatePromise) }, "AgentActor")
    agentActor
  }
}
