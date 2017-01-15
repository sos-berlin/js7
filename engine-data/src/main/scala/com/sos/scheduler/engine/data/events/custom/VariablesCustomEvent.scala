package com.sos.scheduler.engine.data.events.custom

import com.sos.scheduler.engine.data.event.KeyedEvent
import scala.collection.JavaConversions._
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
final case class VariablesCustomEvent(variables: Map[String, String]) extends CustomEvent

object VariablesCustomEvent {
  implicit val jsonFormat = jsonFormat1(apply)

  // For Java
  def of(variables: java.util.Map[String, String]) = new VariablesCustomEvent(variables.toMap)

  // For Java
  def keyed(key: String, variables: java.util.Map[String, String]): KeyedEvent[VariablesCustomEvent] =
    KeyedEvent(of(variables))(CustomEvent.Key(key))
}
