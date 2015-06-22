package com.sos.scheduler.engine.agent.command

import com.google.inject.ImplementedBy
import scala.collection.immutable
import spray.json._

/**
 * @author Joacim Zschimmer
 */
@ImplementedBy(classOf[AgentCommandHandler])
trait CommandHandlerDetails  {
  def commandRuns: immutable.Iterable[CommandRunOverview]
}

object CommandHandlerDetails {
  implicit object MyJsonWriter extends RootJsonWriter[CommandHandlerDetails] {
    def write(o: CommandHandlerDetails) = JsObject(
      "commandRuns" → JsArray((o.commandRuns map { _.toJson }).toVector))
  }
}
