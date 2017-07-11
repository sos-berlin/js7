package com.sos.jobscheduler.agent.scheduler.order

import akka.actor.{ActorRef, Props}
import akka.util.Timeout
import com.google.common.io.Closer
import com.google.inject.{AbstractModule, Guice}
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.configuration.Akkas.newActorSystem
import com.sos.jobscheduler.agent.configuration.inject.AgentModule
import com.sos.jobscheduler.agent.data.commands.AgentCommand
import com.sos.jobscheduler.agent.scheduler.AgentActor
import com.sos.jobscheduler.agent.scheduler.job.task.TaskRunner
import com.sos.jobscheduler.agent.scheduler.order.TestAgentActorProvider._
import com.sos.jobscheduler.agent.test.AgentDirectoryProvider
import com.sos.jobscheduler.base.generic.Completed
import com.sos.jobscheduler.common.auth.User.Anonymous
import com.sos.jobscheduler.common.event.EventIdGenerator
import com.sos.jobscheduler.common.event.collector.EventCollector
import com.sos.jobscheduler.common.guice.GuiceImplicits.RichInjector
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Closers.implicits.RichClosersAutoCloseable
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.HasCloser
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.timer.TimerService
import com.sos.jobscheduler.shared.event.{ActorEventCollector, StampedKeyedEventBus}
import java.nio.file.Path
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}

/**
  * @author Joacim Zschimmer
  */
private class TestAgentActorProvider extends HasCloser {
  private val directoryProvider = new AgentDirectoryProvider {} .provideAgent2Directories().closeWithCloser
  lazy val agentDirectory = directoryProvider.agentDirectory

  lazy val (eventCollector, agentActor, terminated) = start(agentDirectory)
  lazy val lastEventId = eventCollector.lastEventId

  def startAgent() = agentActor

  def executeCommand(command: AgentCommand): Future[AgentCommand.Response] = {
    val response = Promise[AgentCommand.Response]()
    agentActor ! AgentActor.Input.ExternalCommand(MasterUserId, command, response)
    response.future
  }
}

object TestAgentActorProvider {
  private val MasterUserId = Anonymous.id

  def provide[A](body: TestAgentActorProvider ⇒ A): A =
    autoClosing(new TestAgentActorProvider)(body)

  private def start(configAndData: Path)(implicit closer: Closer): (ActorEventCollector, ActorRef, Future[Completed]) = {
    val agentConfiguration = AgentConfiguration.forTest(configAndData = Some(configAndData))
    val actorSystem = newActorSystem("TestAgentActorProvider")
    val injector = Guice.createInjector(new AgentModule(agentConfiguration))
    implicit val newTaskRunner = injector.instance[TaskRunner.Factory]
    implicit val timerService = TimerService(idleTimeout = Some(1.s))
    implicit val keyedEventBus = injector.instance[StampedKeyedEventBus]
    implicit val eventIdGenerator = injector.instance[EventIdGenerator]

    val eventCollector = injector.createChildInjector(new AbstractModule {
      def configure() = bind(classOf[EventCollector.Configuration]) toInstance
        new EventCollector.Configuration(queueSize = 100000, timeoutLimit = 99.s)
    }).instance[ActorEventCollector]

    val stopped = Promise[Completed]()
    val agentActor = actorSystem.actorOf(
      Props { new AgentActor(
        stateDirectory = configAndData / "data",
        jobConfigurationDirectory = configAndData / "config" / "live",
        askTimeout = Timeout(30.seconds),
        syncOnCommit = false,
        stoppedPromise = stopped) },
      "AgentActor")

    (eventCollector, agentActor, stopped.future)
  }
}
