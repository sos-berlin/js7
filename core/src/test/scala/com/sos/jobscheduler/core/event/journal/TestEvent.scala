package com.sos.jobscheduler.core.event.journal

import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.data.event.Event
import io.circe.generic.JsonCodec

/**
  * @author Joacim Zschimmer
  */
private[journal] sealed trait TestEvent extends Event {
  type Key = String
}

private[journal] object TestEvent {
  @JsonCodec
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

  @JsonCodec
  final case class Appended(char: Char) extends TestEvent

  final case object Removed extends TestEvent

  implicit val OrderEventJsonFormat = TypedJsonCodec[TestEvent](
    Subtype[Added],
    Subtype[Appended],
    Subtype(Removed))
}
