package com.sos.jobscheduler.core.event.journal.watch

import com.sos.jobscheduler.core.common.jsonseq.PositionAnd
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class EventIdPositionIndexTest extends FreeSpec {

  private val index = new EventIdPositionIndex(6)

  "No overflow" in {
    intercept[IllegalStateException] { index.positionAfter(0) }
    assert(index.positionAndEventIds.isEmpty)

    index.addAfter(1, 100)
    intercept[IllegalArgumentException] { index.positionAfter(0) }
    assert(index.positionAfter(1) == 100)
    assert(index.positionAfter(2) == 100)
    assert(index.positionAfter(3) == 100)
    assert(index.positionAndEventIds == Vector(
      PositionAnd(100, 1)))

    index.addAfter(2, 200)
    assert(index.positionAfter(1) == 100)
    assert(index.positionAfter(2) == 200)
    assert(index.positionAfter(3) == 200)
    assert(index.positionAndEventIds == Vector(
      PositionAnd(100, 1),
      PositionAnd(200, 2)))

    index.addAfter(3, 300)
    assert(index.positionAfter(3) == 300)
    index.addAfter(4, 400)
    index.addAfter(5, 500)
    index.addAfter(6, 600)
    assert(index.positionAndEventIds == Vector(
      PositionAnd(100, 1),
      PositionAnd(200, 2),
      PositionAnd(300, 3),
      PositionAnd(400, 4),
      PositionAnd(500, 5),
      PositionAnd(600, 6)))
  }

  "Overflow" in {
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
    assert(index.positionAndEventIds == Vector(
      PositionAnd(100, 1),
      PositionAnd(300, 3),
      PositionAnd(500, 5),
      PositionAnd(700, 7)))
  }

  "Adding more" in {
    index.addAfter(8, 800)
    index.addAfter(9, 900)
    assert(index.positionAndEventIds == Vector(
      PositionAnd(100, 1),
      PositionAnd(300, 3),
      PositionAnd(500, 5),
      PositionAnd(700, 7),
      PositionAnd(800, 8),
      PositionAnd(900, 9)))
  }

  "Overflow #2" in {
    index.addAfter(10, 1000)
    assert(index.positionAndEventIds == Vector(
      PositionAnd( 100,  1),
      PositionAnd( 500,  5),
      PositionAnd( 800,  8),
      PositionAnd(1000, 10)))
  }

  "Adding more #2" in {
    index.addAfter(11, 1100)
    index.addAfter(12, 1200)
    assert(index.positionAndEventIds == Vector(
      PositionAnd( 100,  1),
      PositionAnd( 500,  5),
      PositionAnd( 800,  8),
      PositionAnd(1000, 10),
      PositionAnd(1100, 11),
      PositionAnd(1200, 12)))
  }

  "Overflow #3" in {
    index.addAfter( 13, 1300)
    assert(index.positionAndEventIds == Vector(
      PositionAnd( 100,  1),
      PositionAnd( 800,  8),
      PositionAnd(1100, 11),
      PositionAnd(1300, 13)))
  }

  "Many more overflows" in {
    for (i ← 14 to 60) index.addAfter(i, i * 100)
    assert(index.positionAndEventIds == Vector(
      PositionAnd( 100,  1),
      PositionAnd(5300, 53),
      PositionAnd(5600, 56),
      PositionAnd(5800, 58),
      PositionAnd(5900, 59),
      PositionAnd(6000, 60)))
  }

  "releaseUnusedMemory" in {
    index.releaseUnusedMemory()
    intercept[IllegalStateException] {
      index.addAfter(99900, 999)
    }
  }
}
