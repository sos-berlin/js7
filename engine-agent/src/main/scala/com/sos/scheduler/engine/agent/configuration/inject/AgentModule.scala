package com.sos.scheduler.engine.agent.configuration.inject

import akka.actor.{ActorRefFactory, ActorSystem}
import com.google.common.io.Closer
import com.google.inject.{Injector, Provides}
import com.sos.scheduler.engine.agent.configuration.AgentConfiguration
import com.sos.scheduler.engine.agent.configuration.AgentConfiguration.UseInternalKillScript
import com.sos.scheduler.engine.agent.configuration.Akkas.newActorSystem
import com.sos.scheduler.engine.agent.data.views.TaskHandlerView
import com.sos.scheduler.engine.agent.task.{StandardAgentTaskFactory, TaskHandler}
import com.sos.scheduler.engine.agent.web.common.ExternalWebService
import com.sos.scheduler.engine.common.guice.ScalaAbstractModule
import com.sos.scheduler.engine.common.scalautil.Closers.implicits._
import com.sos.scheduler.engine.common.time.timer.TimerService
import com.sos.scheduler.engine.taskserver.moduleapi.ModuleFactoryRegister
import com.sos.scheduler.engine.taskserver.modules
import com.sos.scheduler.engine.taskserver.modules.StandardModuleFactories
import com.sos.scheduler.engine.taskserver.modules.javamodule.{JavaModule, StandardJavaModule}
import com.sos.scheduler.engine.taskserver.modules.shell.ShellModule
import com.sos.scheduler.engine.taskserver.task.process.ProcessKillScriptProvider
import com.typesafe.config.ConfigFactory
import javax.inject.Singleton
import scala.collection.immutable
import scala.concurrent.ExecutionContext

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
  private def moduleFactoryRegister(): ModuleFactoryRegister = {
    val moduleFactories =
      if (StandardAgentTaskFactory.runInProcess) StandardModuleFactories  // For in-process debugging
      else List(ShellModule)  // Other modules get its own process via TaskServerMain
    new ModuleFactoryRegister(moduleFactories)
  }

  @Provides @Singleton
  private def timerService(actorSystem: ActorSystem, closer: Closer): TimerService =
    TimerService()(actorSystem.dispatcher) closeWithCloser closer

  @Provides @Singleton
  private def actorSystem(closer: Closer, agentConfiguration: AgentConfiguration): ActorSystem = {
    var config = ConfigFactory.empty
    if (agentConfiguration.https.isDefined) {
      config = ConfigFactory.parseString("spray.can.server.ssl-encryption = on") withFallback config
    }
    newActorSystem("Agent", config)(closer)
  }

  @Provides @Singleton
  private def executionContext(actorSystem: ActorSystem): ExecutionContext = actorSystem.dispatcher

  @Provides @Singleton
  private def agentConfiguration(): AgentConfiguration =
    if (originalAgentConfiguration.killScript contains UseInternalKillScript) {
      // After Agent termination, leave behind the kill script, in case of regular termination after error.
      val identifiyingPort = originalAgentConfiguration.https map { _.port } orElse originalAgentConfiguration.httpPort getOrElse 0
      val provider = new ProcessKillScriptProvider(httpPort = identifiyingPort)
      val killScript = provider.provideTo(originalAgentConfiguration.logDirectory)  // logDirectory for lack of a work directory
      originalAgentConfiguration.copy(killScript = Some(killScript))
    } else
      originalAgentConfiguration

  @Provides @Singleton
  private def closer(): Closer = Closer.create()  // Do not use concurrently !!!
}
