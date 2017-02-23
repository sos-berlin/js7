package com.sos.scheduler.engine.shared.event.journal.tests

import com.sos.scheduler.engine.shared.event.journal.tests.TestEvent._
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
private[tests] case class TestAggregate(key: String, string: String) {

  def update(event: TestEvent) = event match {
    case Appended(o) ⇒ copy(string = string + o)
    case _ ⇒ sys.error(s"Not applicable: $event")
  }
}

private[tests] object TestAggregate {
  implicit val jsonFormat = jsonFormat2(apply)
}
