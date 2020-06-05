package js7.core.event.journal.test

import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.data.event.Event
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

  final case object NothingDone extends TestEvent

  final case object Removed extends TestEvent

  implicit val jsonFormat = TypedJsonCodec[TestEvent](
    Subtype[Added],
    Subtype[Appended],
    Subtype(NothingDone),
    Subtype(Removed))
}
