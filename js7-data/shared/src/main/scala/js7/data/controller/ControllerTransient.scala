package js7.data.controller

import js7.base.time.Throttle
import org.jetbrains.annotations.TestOnly

/** Data that is not persisted.
  *
  * No Events, no snapshot. Values vanish after Controller restart.
  */
final case class ControllerTransient(
  addOrderInstrThrottle: Throttle)


object ControllerTransient:

  @TestOnly
  def forTest: ControllerTransient =
    new ControllerTransient(Throttle.Unlimited)
