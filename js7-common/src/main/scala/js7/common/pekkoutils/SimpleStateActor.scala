package js7.common.pekkoutils

import org.apache.pekko.actor.Actor

/**
  * @author Joacim Zschimmer
  */
trait SimpleStateActor extends Actor:
  private var _actorStateName: String = ""

  protected def become(stateName: String)(receive: Receive): Unit =
    _actorStateName = stateName
    context.become(receive)

  final def actorStateName: String = _actorStateName
