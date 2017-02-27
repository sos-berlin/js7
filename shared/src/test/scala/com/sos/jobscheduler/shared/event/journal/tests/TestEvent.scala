package com.sos.jobscheduler.shared.event.journal.tests

import com.sos.jobscheduler.base.sprayjson.typed.{Subtype, TypedJsonFormat}
import com.sos.jobscheduler.data.event.Event
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
private[tests] sealed trait TestEvent extends Event {
  type Key = String
}

private[tests] object TestEvent {
  final case class Added(
    string: String,
    a: String = "X",   // Many arguments for speed test
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
    r: String = "X")
  extends TestEvent

  final case class Appended(string: String) extends TestEvent

  final case object Removed extends TestEvent

  implicit val OrderEventJsonFormat = TypedJsonFormat[TestEvent](
    Subtype(jsonFormat19(Added)),
    Subtype(jsonFormat1(Appended)),
    Subtype(jsonFormat0(() ⇒ Removed)))
}
