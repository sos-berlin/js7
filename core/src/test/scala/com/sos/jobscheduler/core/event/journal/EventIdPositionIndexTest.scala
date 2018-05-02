package com.sos.jobscheduler.core.event.journal

import com.sos.jobscheduler.core.common.jsonseq.PositionAnd
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class EventIdPositionIndexTest extends FreeSpec {

  "test" in {
    val index = new EventIdPositionIndex(6)
    intercept[IllegalStateException] { index.positionAfter(0) }

    index.addAfter(1, 100)
    intercept[IllegalArgumentException] { index.positionAfter(0) }
    assert(index.positionAfter(1) == 100)
    assert(index.positionAfter(2) == 100)
    assert(index.positionAfter(3) == 100)

    index.addAfter(2, 200)
    assert(index.positionAfter(1) == 100)
    assert(index.positionAfter(2) == 200)
    assert(index.positionAfter(3) == 200)

    index.addAfter(3, 300)
    assert(index.positionAfter(3) == 300)
    index.addAfter(4, 400)
    index.addAfter(5, 500)
    index.addAfter(6, 600)

    index.addAfter(7, 700)  // Index overflows
    intercept[IllegalArgumentException] { index.positionAfter(0) }
    assert(index.positionAfter(1) == 100)
    assert(index.positionAfter(2) == 100)
    assert(index.positionAfter(3) == 300)
    assert(index.positionAfter(4) == 300)
    assert(index.positionAfter(5) == 500)
    assert(index.positionAfter(6) == 500)
    assert(index.positionAfter(7) == 700)
    assert(index.positionAfter(8) == 700)
  }
}
