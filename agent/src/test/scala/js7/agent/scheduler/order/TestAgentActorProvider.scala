package js7.agent.scheduler.order

import akka.actor.{ActorRef, Props}
import com.google.inject.{AbstractModule, Guice, Provides}
import js7.agent.configuration.AgentConfiguration
import js7.agent.configuration.Akkas.newAgentActorSystem
import js7.agent.configuration.inject.AgentModule
import js7.agent.data.AgentTermination
import js7.agent.data.commands.AgentCommand
import js7.agent.scheduler.AgentActor
import js7.agent.scheduler.order.TestAgentActorProvider._
import js7.agent.tests.TestAgentDirectoryProvider
import js7.base.auth.UserId
import js7.base.problem.Checked
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.Closer.syntax.RichClosersAutoCloseable
import js7.base.utils.{Closer, HasCloser}
import js7.common.event.collector.EventCollector
import js7.common.guice.GuiceImplicits.RichInjector
import js7.core.event.ActorEventCollector
import java.nio.file.Path
import javax.inject.Singleton
import scala.concurrent.{Future, Promise}

/**
  * @author Joacim Zschimmer
  */
private class TestAgentActorProvider extends HasCloser {
  private val directoryProvider = TestAgentDirectoryProvider().closeWithCloser
  lazy val agentDirectory = directoryProvider.agentDirectory

  lazy val (eventCollector, agentActor, _) = start(agentDirectory)

  def startAgent() = agentActor

  def fileBasedSigner = directoryProvider.fileBasedSigner

  def executeCommand(command: AgentCommand): Future[Checked[AgentCommand.Response]] = {
    val response = Promise[Checked[AgentCommand.Response]]()
    agentActor ! AgentActor.Input.ExternalCommand(MasterUserId, command, response)
    response.future
  }
}

object TestAgentActorProvider {
  private val MasterUserId = UserId.Anonymous

  def provide[A](body: TestAgentActorProvider => A): A =
    autoClosing(new TestAgentActorProvider)(body)

  private def start(configAndData: Path)(implicit closer: Closer): (EventCollector, ActorRef, Future[AgentTermination.Terminate]) = {
    implicit val agentConfiguration = AgentConfiguration.forTest(configAndData = configAndData)
    val actorSystem = newAgentActorSystem("TestAgentActorProvider")
    val injector = Guice.createInjector(new AgentModule(agentConfiguration))

    val eventCollector = injector.createChildInjector(new AbstractModule {
      override def configure() = bind(classOf[EventCollector.Configuration]) toInstance
        new EventCollector.Configuration(queueSize = 100000)

      @Provides @Singleton
      def eventCollector(factory: ActorEventCollector.Factory): EventCollector =
        factory.apply()
    }).instance[EventCollector]

    val terminatePromise = Promise[AgentTermination.Terminate]()
    val agentActor = actorSystem.actorOf(Props { injector.instance[AgentActor.Factory].apply(terminatePromise) }, "AgentActor")
    (eventCollector, agentActor, terminatePromise.future)
  }
}
