package com.sos.scheduler.engine.agent.command

import com.google.inject.ImplementedBy
import spray.json.{JsNumber, JsObject, RootJsonWriter}

/**
 * @author Joacim Zschimmer
 */
@ImplementedBy(classOf[AgentCommandHandler])
trait CommandHandlerOverview {
  def totalCommandCount: Int
  def currentCommandCount: Int
}

object CommandHandlerOverview {
  implicit object MyJsonWriter extends RootJsonWriter[CommandHandlerOverview] {
    def write(o: CommandHandlerOverview) = JsObject(
      "totalCommandCount" → JsNumber(o.totalCommandCount),
      "currentCommandCount" → JsNumber(o.currentCommandCount))
  }
}
