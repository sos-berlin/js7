package js7.data.controller

import js7.base.time.SpeedLimiter
import org.jetbrains.annotations.TestOnly

/** Data that is not persisted.
  *
  * No Events, no snapshot. Values vanish after Controller restart.
  */
final case class ControllerVolatile(
  addOrderInstrSpeedLimiter: SpeedLimiter)


object ControllerVolatile:

  @TestOnly
  def forTest: ControllerVolatile =
    new ControllerVolatile(SpeedLimiter.Unlimited)
