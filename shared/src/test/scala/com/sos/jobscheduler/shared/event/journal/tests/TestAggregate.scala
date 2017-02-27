package com.sos.jobscheduler.shared.event.journal.tests

import com.sos.jobscheduler.shared.event.journal.tests.TestEvent._
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
private[tests] case class TestAggregate(key: String, string: String,
  a: String = "X",
  b: String = "X",
  c: String = "X",
  d: String = "X",
  e: String = "X",
  f: String = "X",
  g: String = "X",
  h: String = "X",
  i: String = "X",
  j: String = "X",
  k: String = "X",
  l: String = "X",
  m: String = "X",
  n: String = "X",
  o: String = "X",
  p: String = "X",
  q: String = "X",
  r: String = "X") {

  def update(event: TestEvent) = event match {
    case Appended(o) ⇒ copy(string = string + o)
    case _ ⇒ sys.error(s"Not applicable: $event")
  }
}

private[tests] object TestAggregate {
  implicit val jsonFormat = jsonFormat20(apply)
}
