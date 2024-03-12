package js7.journal

import js7.base.time.WallClock

/**
  * @author Joacim Zschimmer
  */
trait EventIdClock:

  /** Current time in milliseconds since 1970-01-01 UTC, like Java currentTimeMillis. */
  def currentTimeMillis: Long
  def isRealClock = false


object EventIdClock:

  @deprecated("Use SystemEventIdClock")
  val Default: EventIdClock =
    SystemEventIdClock

  //@deprecated
  def apply(clock: WallClock): EventIdClock =
    new EventIdClock:
      def currentTimeMillis = clock.epochMilli()
      override def isRealClock = clock eq WallClock

  def fixed(epochMilli: Long) =
    new EventIdClock:
      val currentTimeMillis = epochMilli

  object SystemEventIdClock extends EventIdClock:
    def currentTimeMillis = System.currentTimeMillis()
    override def isRealClock = true
