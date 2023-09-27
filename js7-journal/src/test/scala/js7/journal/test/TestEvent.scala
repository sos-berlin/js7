package js7.journal.test

import io.circe.generic.semiauto.deriveCodec
import js7.base.circeutils.typed.{Subtype, TypedJsonCodec}
import js7.data.event.Event

/**
  * @author Joacim Zschimmer
  */
private[journal] sealed trait TestEvent extends Event.IsKeyBase[TestEvent] {
  val keyCompanion: TestEvent.type = TestEvent
}

private[journal] object TestEvent extends Event.CompanionForKey[String, TestEvent] {
  implicit val implicitSelf: TestEvent.type = this

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

  final case class Appended(char: Char) extends TestEvent

  case object NothingDone extends TestEvent

  case object Removed extends TestEvent

  implicit val jsonFormat: TypedJsonCodec[TestEvent] = TypedJsonCodec(
    Subtype(deriveCodec[Added]),
    Subtype(deriveCodec[Appended]),
    Subtype(NothingDone),
    Subtype(Removed))
}
