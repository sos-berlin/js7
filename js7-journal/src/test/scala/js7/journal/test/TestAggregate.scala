package js7.journal.test

import io.circe.generic.semiauto.deriveCodec
import js7.journal.test.TestEvent.{Appended, NothingDone}

/**
  * @author Joacim Zschimmer
  */
private[journal] final case class TestAggregate(key: String, string: String,
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
  r: String = "X")
{

  def applyEvent(event: TestEvent) = event match {
    case Appended(char) => copy(string = string + char)
    case NothingDone => this
    case _ => sys.error(s"Not applicable: $event")
  }
}

private[journal] object TestAggregate {
  implicit val jsonCodec = deriveCodec[TestAggregate]
}
