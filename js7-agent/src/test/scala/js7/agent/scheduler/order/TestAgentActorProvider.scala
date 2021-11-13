package js7.agent.scheduler.order

import akka.actor.{ActorRef, ActorSystem, Props}
import com.google.inject.Guice
import com.softwaremill.diffx.generic.auto._
import java.nio.file.Path
import js7.agent.configuration.AgentConfiguration
import js7.agent.configuration.inject.AgentModule
import js7.agent.data.AgentState
import js7.agent.data.commands.AgentCommand
import js7.agent.scheduler.AgentActor
import js7.agent.scheduler.order.TestAgentActorProvider._
import js7.agent.tests.TestAgentDirectoryProvider
import js7.base.auth.UserId
import js7.base.problem.Checked
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.utils.AutoClosing.autoClosing
import js7.base.utils.Closer.syntax.RichClosersAutoCloseable
import js7.base.utils.{Closer, HasCloser}
import js7.common.guice.GuiceImplicits.RichInjector
import js7.journal.recover.Recovered
import js7.journal.state.{FileStatePersistence, StatePersistence}
import js7.journal.{EventIdGenerator, StampedKeyedEventBus}
import monix.execution.Scheduler
import scala.concurrent.duration.Deadline.now
import scala.concurrent.{Future, Promise}

/**
  * @author Joacim Zschimmer
  */
private class TestAgentActorProvider(testName: String) extends HasCloser
{
  private val directoryProvider = TestAgentDirectoryProvider().closeWithCloser
  lazy val agentDirectory = directoryProvider.agentDirectory

  lazy val (agentConfiguration, persistence, agentActor) =
    start(agentDirectory, testName)

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

  private def start(configAndData: Path, testName: String)(implicit closer: Closer)
  : (AgentConfiguration, StatePersistence[AgentState], ActorRef) = {
    implicit val agentConfiguration = AgentConfiguration.forTest(configAndData = configAndData, name = testName)
    import agentConfiguration.{akkaAskTimeout, config, journalConf, journalMeta}
    val injector = Guice.createInjector(new AgentModule(agentConfiguration))

    // Initialize Akka here to solve a classloader problem when Akka reads its reference.conf
    implicit val actorSystem = injector.instance[ActorSystem]
    implicit val scheduler = injector.instance[Scheduler]

    val persistence = FileStatePersistence
      .start(
        Recovered[AgentState](journalMeta, None, now, config),
        journalConf,
        injector.instance[EventIdGenerator],
        injector.instance[StampedKeyedEventBus])
      .awaitInfinite

    val actor = actorSystem.actorOf(
      Props {
        injector
          .instance[AgentActor.Factory]
          .apply(persistence, Promise())
      },
      "AgentActor")

    (agentConfiguration, persistence, actor)
  }
}
