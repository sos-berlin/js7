package com.sos.jobscheduler.agent.scheduler.order

import akka.actor.{ActorRef, Props}
import akka.util.Timeout
import com.google.common.io.Closer
import com.google.inject.{AbstractModule, Guice}
import com.sos.jobscheduler.agent.configuration.AgentConfiguration
import com.sos.jobscheduler.agent.configuration.Akkas.newActorSystem
import com.sos.jobscheduler.agent.configuration.inject.AgentModule
import com.sos.jobscheduler.agent.data.commandresponses.Response
import com.sos.jobscheduler.agent.data.commands.Command
import com.sos.jobscheduler.agent.scheduler.AgentActor
import com.sos.jobscheduler.agent.scheduler.order.TestAgentEnvironment._
import com.sos.jobscheduler.agent.task.AgentTaskFactory
import com.sos.jobscheduler.agent.test.AgentDirectoryProvider
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
import java.nio.file.Files.createDirectory
import java.nio.file.Path
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Future, Promise}

/**
  * @author Joacim Zschimmer
  */
private class TestAgentEnvironment extends HasCloser {
  private val directoryProvider = new AgentDirectoryProvider {} .closeWithCloser
  val directory = directoryProvider.agentDirectory

  createDirectory(directory / "config" / "live")
  createDirectory(directory / "data")
  val subdir =  directory / "config" / "live" / "folder"
  createDirectory(subdir)

  lazy val (eventCollector, agentActor) = start(directory)
  lazy val lastEventId = eventCollector.lastEventId

  def startAgent() = agentActor

  def executeCommand(command: Command): Future[Response] = {
    val response = Promise[Response]()
    agentActor ! AgentActor.Input.ExternalCommand(MasterUserId, command, response)
    response.future
  }
}

object TestAgentEnvironment {
  private val MasterUserId = Anonymous.id

  def provide[A](body: TestAgentEnvironment ⇒ A): A =
    autoClosing(new TestAgentEnvironment)(body)

  private def start(configAndData: Path)(implicit closer: Closer): (EventCollector, ActorRef) = {
    val agentConfiguration = AgentConfiguration.forTest(configAndData = Some(configAndData))
    val actorSystem = newActorSystem("TestAgentEnvironment")
    val injector = Guice.createInjector(new AgentModule(agentConfiguration))
    implicit val agentTaskFactory = injector.instance[AgentTaskFactory]
    implicit val timerService = TimerService(idleTimeout = Some(1.s))
    implicit val keyedEventBus = injector.instance[StampedKeyedEventBus]
    implicit val eventIdGenerator = injector.instance[EventIdGenerator]

    val eventCollector = injector.createChildInjector(new AbstractModule {
      def configure() = bind(classOf[EventCollector.Configuration]) toInstance
        new EventCollector.Configuration(queueSize = 100000, timeoutLimit = 99.s)
    }).instance[ActorEventCollector]

    val agentActor = actorSystem.actorOf(
      Props { new AgentActor(
        stateDirectory = configAndData / "data",
        jobConfigurationDirectory = configAndData / "config" / "live",
        askTimeout = Timeout(30.seconds),
        syncOnCommit = false) },
      "AgentActor")

    (eventCollector, agentActor)
  }
}
