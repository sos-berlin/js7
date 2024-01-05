package js7.common.pekkoutils

import org.apache.pekko.actor.SupervisorStrategy
import org.apache.pekko.actor.SupervisorStrategy.Escalate

/**
  * @author Joacim Zschimmer
  */
object SupervisorStrategies:

  def escalate: SupervisorStrategy =
    new LoggingOneForOneStrategy()({
      case _: Throwable => Escalate
    })
