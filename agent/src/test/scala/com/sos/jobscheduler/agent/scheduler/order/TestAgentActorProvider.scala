package com.sos.jobscheduler.agent.scheduler.order

import akka.actor.{ActorRef, Props}
import com.google.inject.{AbstractModule, Guice, Provides}
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.configuration.Akkas.newAgentActorSystem
import com.sos.jobscheduler.agent.configuration.inject.AgentModule
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.scheduler.AgentActor
import com.sos.jobscheduler.agent.scheduler.order.TestAgentActorProvider._
import com.sos.jobscheduler.agent.tests.TestAgentDirectoryProvider
import com.sos.jobscheduler.base.auth.UserId
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.common.event.collector.EventCollector
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Closer.ops.RichClosersAutoCloseable
import com.sos.jobscheduler.common.scalautil.{Closer, HasCloser}
import com.sos.jobscheduler.core.event.ActorEventCollector
import java.nio.file.Path
import javax.inject.Singleton
import scala.concurrent.{Future, Promise}

/**
  * @author Joacim Zschimmer
  */
private class TestAgentActorProvider extends HasCloser {
  private val directoryProvider = TestAgentDirectoryProvider().closeWithCloser
  lazy val agentDirectory = directoryProvider.agentDirectory

  lazy val (eventCollector, agentActor) = start(agentDirectory)

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

  private def start(configAndData: Path)(implicit closer: Closer): (EventCollector, ActorRef) = {
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

    val agentActor = actorSystem.actorOf(Props { injector.instance[AgentActor] }, "AgentActor")
    (eventCollector, agentActor)
  }
}
