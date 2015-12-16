package com.sos.scheduler.engine.agent.configuration.inject

import akka.actor.{ActorRefFactory, ActorSystem}
import com.google.common.io.Closer
import com.google.inject.{Injector, Provides}
import com.sos.scheduler.engine.agent.configuration.AgentConfiguration
import com.sos.scheduler.engine.agent.configuration.Akkas.newActorSystem
import com.sos.scheduler.engine.agent.data.views.TaskHandlerView
import com.sos.scheduler.engine.agent.task.TaskHandler
import com.sos.scheduler.engine.agent.web.common.ExternalWebService
import com.sos.scheduler.engine.common.guice.ScalaAbstractModule
import com.sos.scheduler.engine.common.scalautil.Closers.implicits._
import com.sos.scheduler.engine.common.time.ScalaTime.DurationRichInt
import com.sos.scheduler.engine.common.time.alarm.AlarmClock
import com.sos.scheduler.engine.taskserver.task.process.ProcessKillScriptProvider
import javax.inject.Singleton
import scala.collection.immutable

/**
 * @author Joacim Zschimmer
 */
final class AgentModule(originalAgentConfiguration: AgentConfiguration) extends ScalaAbstractModule {

  protected def configure() = {}

  @Provides @Singleton
  private def extraWebServices(agentConfiguration: AgentConfiguration, injector: Injector): immutable.Seq[ExternalWebService] =
    agentConfiguration.externalWebServiceClasses map { o ⇒ injector.getInstance(o) }

  @Provides @Singleton
  private def actorRefFactory(o: ActorSystem): ActorRefFactory = o

  @Provides @Singleton
  private def taskHandlerView(o: TaskHandler): TaskHandlerView = o

  @Provides @Singleton
  private def actorSystem(closer: Closer): ActorSystem = newActorSystem("JobScheduler-Agent")(closer)

  @Provides @Singleton
  private def alarmClock(closer: Closer): AlarmClock = new AlarmClock(100.ms) closeWithCloser closer

  @Provides @Singleton
  private def agentConfiguration(): AgentConfiguration =
    if (originalAgentConfiguration.killScript.nonEmpty)
      originalAgentConfiguration
    else {
      val provider = new ProcessKillScriptProvider // closeWithCloser closer
      // After Agent termination, leave behind the kill script, in case of regular termination after error.
      val killScript = provider.provideTo(originalAgentConfiguration.logDirectory)  // logDirectory for lack of a work directory
      originalAgentConfiguration.copy(killScript = Some(killScript))
    }

  @Provides @Singleton
  private def closer: Closer = Closer.create()
}
