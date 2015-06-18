package com.sos.scheduler.engine.agent.process

import com.google.inject.ImplementedBy
import com.sos.scheduler.engine.agent.data.views.ProcessOverview
import scala.collection.immutable
import spray.json.DefaultJsonProtocol._
import spray.json._

/**
 * @author Joacim Zschimmer
 */
@ImplementedBy(classOf[ProcessHandler])
trait ProcessHandlerView {

  def currentProcessCount: Int
  
  def totalProcessCount: Int

  def processes: immutable.Iterable[ProcessOverview]

  def isTerminating: Boolean
}

object ProcessHandlerView {
  implicit object MyJsonWriter extends RootJsonWriter[ProcessHandlerView] {
    def write(o: ProcessHandlerView) = JsObject(
      "processes" → o.processes.toJson)
  }
}
