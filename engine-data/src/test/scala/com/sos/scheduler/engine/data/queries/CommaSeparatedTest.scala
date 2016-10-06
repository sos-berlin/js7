package com.sos.scheduler.engine.data.queries

import com.sos.scheduler.engine.data.queries.CommaSeparated.commaSplittedAsSet
import com.sos.scheduler.engine.data.queries.CommaSeparated.toNamedCommaSeparated
import com.sos.scheduler.engine.data.queries.CommaSeparated.toCommaSeparated
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class CommaSeparatedTest extends FreeSpec {

  "toNamedCommaSeparated" in {
    assert(toNamedCommaSeparated("NAME", Some(List(true, false)))(_.toString) == Some("NAME" → "true,false"))
    assert(toNamedCommaSeparated("NAME", None)((o: Boolean) ⇒ o.toString) == None)
    intercept[IllegalArgumentException] {
      toNamedCommaSeparated("NAME", Some(List("1,2")))(identity)
    }
  }

  "toCommaSeparated" in {
    assert(toCommaSeparated(List(true, false))(_.toString) == "true,false")
    assert(toCommaSeparated(List(1, 2))(o ⇒ s"($o)") == "(1),(2)")
    intercept[IllegalArgumentException] {
      toCommaSeparated(List("1,2"))(identity)
    }
  }

  "commaSplittedAsSet" in {
    val asInts = commaSplittedAsSet(_.toInt)
    assert(asInts("") == Set())
    assert(asInts("1") == Set(1))
    assert(asInts("1") == Set(1))
    assert(asInts("1") == Set(1))
    assert(asInts("1,22,333") == Set(1,22,333))
    assert(asInts("1,22,333") == Set(1,22,333))
    intercept[IllegalArgumentException] { asInts(" ") }
    intercept[IllegalArgumentException] { asInts(",") }
    intercept[IllegalArgumentException] { asInts(" 1") }
    intercept[IllegalArgumentException] { asInts("1 ") }
    intercept[IllegalArgumentException] { asInts("1,") }
    intercept[IllegalArgumentException] { asInts("1, 2") }
  }
}
