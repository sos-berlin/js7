package js7.controller.configuration.inject

import com.google.inject.{AbstractModule, Provides}
import com.typesafe.config.Config
import javax.inject.Singleton
import js7.base.eventbus.StandardEventBus
import js7.base.log.{CorrelId, Logger}
import js7.base.thread.IOExecutor
import js7.base.thread.ThreadPoolsBase.newBlockingExecutor
import js7.base.time.JavaTimeConverters.*
import js7.base.time.{AlarmClock, WallClock}
import js7.base.utils.Closer
import js7.common.system.ThreadPools
import js7.controller.configuration.ControllerConfiguration
import js7.journal.{EventIdClock, EventIdGenerator}
import monix.execution.Scheduler

/**
  * @author Joacim Zschimmer
  */
final class ControllerModule(
  configuration: ControllerConfiguration,
  commonScheduler: Option[Scheduler] = None)
extends AbstractModule
{
  import configuration.config

  @Provides @Singleton
  def eventIdGenerator(eventIdClock: EventIdClock): EventIdGenerator =
    new EventIdGenerator(eventIdClock)

  @Provides @Singleton
  def eventIdClock(clock: WallClock): EventIdClock =
    EventIdClock(clock)

  @Provides @Singleton
  def scheduler(closer: Closer): Scheduler =
    commonScheduler.map(CorrelId.enableScheduler(_)) getOrElse
      ThreadPools.newStandardScheduler(configuration.name, config, closer)

  @Provides @Singleton
  def ioExecutor(closer: Closer, conf: ControllerConfiguration, config: Config): IOExecutor = {
    val name = conf.name + "-I/O"
    val threadPool = newBlockingExecutor(config, name)
    closer.onClose { threadPool.shutdown() }
    new IOExecutor(threadPool, name)
  }

  @Provides @Singleton
  def testEventBus(): StandardEventBus[Any] =
    new StandardEventBus[Any]

  @Provides @Singleton
  def provideConfig(): Config =
    config

  @Provides @Singleton
  def controllerConfiguration(): ControllerConfiguration =
    configuration

  /** Do not override this, override alarmClock! */
  @Provides @Singleton
  def wallClock(clock: AlarmClock): WallClock =
    clock

  @Provides @Singleton
  def alarmClock(config: Config)(implicit s: Scheduler): AlarmClock =
    AlarmClock(Some(config
      .getDuration("js7.time.clock-setting-check-interval")
      .toFiniteDuration))

  @Provides @Singleton
  def closer(): Closer =
    new Closer
}

object ControllerModule {
  private val logger = Logger(getClass)
}
