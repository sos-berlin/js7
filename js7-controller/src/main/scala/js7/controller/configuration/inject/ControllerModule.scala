package js7.controller.configuration.inject

import com.google.inject.{AbstractModule, Provides}
import javax.inject.Singleton
import js7.base.time.JavaTimeConverters.*
import js7.base.time.{AlarmClock, WallClock}
import js7.controller.configuration.ControllerConfiguration
import js7.journal.EventIdClock
import monix.execution.Scheduler

/**
  * @author Joacim Zschimmer
  */
final class ControllerModule(
  configuration: ControllerConfiguration,
  scheduler: Scheduler)
extends AbstractModule
{
  @Provides @Singleton
  private def eventIdClock(clock: AlarmClock): EventIdClock =
    EventIdClock(clock: WallClock)

  @Provides @Singleton
  private def alarmClock: AlarmClock =
    AlarmClock(Some(configuration.config
      .getDuration("js7.time.clock-setting-check-interval")
      .toFiniteDuration))(scheduler)
}
