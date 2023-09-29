package js7.common.akkautils

import akka.actor.SupervisorStrategy
import akka.actor.SupervisorStrategy.Escalate

/**
  * @author Joacim Zschimmer
  */
object SupervisorStrategies:

  def escalate: SupervisorStrategy =
    new LoggingOneForOneStrategy()({
      case _: Throwable => Escalate
    })
