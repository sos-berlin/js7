package com.sos.scheduler.engine.agent.task

import com.google.inject.ImplementedBy
import com.sos.scheduler.engine.agent.data.views.TaskOverview
import scala.collection.immutable
import spray.json.DefaultJsonProtocol._
import spray.json._

/**
 * @author Joacim Zschimmer
 */
@ImplementedBy(classOf[TaskHandler])
trait TaskHandlerView {

  def currentTaskCount: Int
  
  def totalTaskCount: Int

  def tasks: immutable.Iterable[TaskOverview]

  def isTerminating: Boolean
}

object TaskHandlerView {
  implicit object MyJsonWriter extends RootJsonWriter[TaskHandlerView] {
    def write(o: TaskHandlerView) = JsObject(
      "tasks" → o.tasks.toJson)
  }
}
