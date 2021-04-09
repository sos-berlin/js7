package js7.agent.configuration.inject

import akka.actor.{ActorRefFactory, ActorSystem}
import com.google.inject.{AbstractModule, Provides}
import com.typesafe.config.Config
import javax.inject.Singleton
import js7.agent.configuration.AgentConfiguration
import js7.agent.configuration.Akkas.newAgentActorSystem
import js7.agent.web.AgentWebServer
import js7.base.auth.SimpleUser
import js7.base.thread.IOExecutor
import js7.base.thread.ThreadPoolsBase.newUnlimitedThreadPool
import js7.base.utils.Closer
import js7.base.utils.Closer.syntax._
import js7.common.akkahttp.web.auth.GateKeeper
import js7.common.akkahttp.web.session.{SessionRegister, SimpleSession}
import js7.common.system.ThreadPools
import js7.common.system.ThreadPools.newUnlimitedScheduler
import js7.core.cluster.ClusterWatchRegister
import js7.executor.configuration.{JobExecutorConf, TaskConfiguration}
import js7.executor.process.{RichProcessStartSynchronizer, SimpleShellTaskRunner}
import js7.executor.task.TaskRunner
import js7.journal.EventIdGenerator
import monix.execution.Scheduler
import monix.execution.schedulers.SchedulerService
import scala.concurrent.ExecutionContext

/**
 * @author Joacim Zschimmer
 */
final class AgentModule(originalAgentConfiguration: AgentConfiguration)
extends AbstractModule
{
  @Provides @Singleton
  def eventIdGenerator(): EventIdGenerator =
    new EventIdGenerator()

  @Provides @Singleton
  def sessionRegister(actorSystem: ActorSystem, config: Config)(implicit s: Scheduler): SessionRegister[SimpleSession] =
    SessionRegister.start[SimpleSession](actorSystem, SimpleSession.apply, config)

  @Provides @Singleton
  def gateKeeperConfiguration(config: Config): GateKeeper.Configuration[SimpleUser] =
    GateKeeper.Configuration.fromConfig(config, SimpleUser.apply)

  @Provides @Singleton
  def actorRefFactory(o: ActorSystem): ActorRefFactory = o

  @Provides @Singleton
  def actorSystem(closer: Closer, conf: AgentConfiguration, config: Config, executionContext: ExecutionContext): ActorSystem =
    newAgentActorSystem(conf.name, config, executionContext)(closer)

  @Provides @Singleton
  def ioExecutor(closer: Closer, conf: AgentConfiguration, config: Config): IOExecutor = {
    val result = new IOExecutor(newUnlimitedThreadPool(config, name = conf.name + " I/O"))
    closer.onClose { result.shutdown() }
    result
  }

  @Provides @Singleton
  def executionContext(scheduler: Scheduler): ExecutionContext =
    scheduler

  @Provides @Singleton
  def monixScheduler(configuration: AgentConfiguration, closer: Closer): Scheduler =
    ThreadPools.newStandardScheduler(configuration.name, configuration.config, closer)

  @Provides @Singleton
  def shellTaskRunnerFactory(
    synchronizedStartProcess: RichProcessStartSynchronizer,
    agentConf: AgentConfiguration)
    (implicit scheduler: Scheduler, iox: IOExecutor)
  : TaskRunner.Factory =
    new TaskRunner.Factory {
      private val taskIdGenerator = new SimpleShellTaskRunner.TaskIdGenerator
      def apply(conf: TaskConfiguration) = {
        val taskId = taskIdGenerator.next()
        new SimpleShellTaskRunner(conf, taskId, synchronizedStartProcess,
          temporaryDirectory = agentConf.temporaryDirectory,
          workingDirectory = agentConf.jobWorkingDirectory,
          killScript = agentConf.killScript)
      }
    }

  @Provides @Singleton
  def executorConf(conf: AgentConfiguration, newTaskRunner: TaskRunner.Factory, closer: Closer)
  : JobExecutorConf = {
    val blockingJobScheduler: SchedulerService = {
      val scheduler = newUnlimitedScheduler("JS7 blocking job")
      closer.onClose(scheduler.shutdown())
      scheduler
    }
    conf.toExecutorConf(newTaskRunner, blockingJobScheduler)
  }

  @Provides @Singleton
  def agentConfiguration(): AgentConfiguration = originalAgentConfiguration.finishAndProvideFiles

  @Provides @Singleton
  def config(agentConfiguration: AgentConfiguration): Config = agentConfiguration.config

  @Provides @Singleton
  def closer(): Closer = new Closer

  @Provides @Singleton
  def provideAgentWebServer(conf: AgentConfiguration, gateKeeperConfiguration: GateKeeper.Configuration[SimpleUser],
    sessionRegister: SessionRegister[SimpleSession],
    clusterWatchRegister: ClusterWatchRegister,
    config: Config,
    actorSystem: ActorSystem, scheduler: Scheduler, closer: Closer): AgentWebServer =
      new AgentWebServer(conf, gateKeeperConfiguration, sessionRegister, clusterWatchRegister,
        config, actorSystem, scheduler
      ).closeWithCloser(closer)
}
