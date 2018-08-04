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
    index.addAfter( 8,  800)
    index.addAfter( 9,  900)
    index.addAfter(10, 1000)
    index.addAfter(11, 1100)
    assert(index.positionAndEventIds == Vector(
      PositionAnd(100, 1),
      PositionAnd(300, 3),
      PositionAnd(500, 5),
      PositionAnd(700, 7),
      PositionAnd(800, 8),
      PositionAnd(1000, 10)))
  }

  "Overflow #2" in {
    index.addAfter(12, 1200)
    assert(index.positionAndEventIds == Vector(
      PositionAnd( 100,  1),
      PositionAnd( 500,  5),
      PositionAnd( 800,  8),
      PositionAnd(1200, 12)))
  }

  "Adding more #2" in {
    for (i ← 13 to 23) index.addAfter(i, i * 100)
    assert(index.positionAndEventIds == Vector(
      PositionAnd( 100,  1),
      PositionAnd( 500,  5),
      PositionAnd( 800,  8),
      PositionAnd(1200, 12),
      PositionAnd(1600, 16),
      PositionAnd(2000, 20)))
  }

  "Overflow #3" in {
    for (i ← 24 to 47) index.addAfter(i, i * 100)
    assert(index.positionAndEventIds == Vector(
      PositionAnd( 100,  1),
      PositionAnd( 800,  8),
      PositionAnd(1600, 16),
      PositionAnd(2400, 24),
      PositionAnd(3200, 32),
      PositionAnd(4000, 40)))
  }

  "Positions are evenly distributed after many more positions have been added" in {
    for (i ← 48 to 6000) index.addAfter(i, i * 100)
    assert(index.positionAndEventIds == Vector(
      PositionAnd(   100,  1),
      PositionAnd(102400, 1024),
      PositionAnd(204800, 2048),
      PositionAnd(307200, 3072),
      PositionAnd(409600, 4096),
      PositionAnd(512000, 5120)))
      assert(index.factorForTest == 1024)
  }

  "freeze" in {
    assert(index.factorForTest == 1024)
    index.freeze(toFactor = 100)
    index.positionAndEventIds == Vector(
      PositionAnd(   100,  1),
      PositionAnd(102400, 1024),
      PositionAnd(204800, 2048),
      PositionAnd(307200, 3072),
      PositionAnd(409600, 4096),
      PositionAnd(512000, 5120))
    intercept[IllegalStateException] {
      index.addAfter(99900, 999)
    }
  }

  "freeze 2" in {
    val index = new EventIdPositionIndex(1000)
    for (i ← 1 to 10000) index.addAfter(i, i * 100)
    assert(index.factorForTest == 16 && index.lengthForTest == 626)
    index.freeze(toFactor = 50)
    assert(index.factorForTest == 48 && index.lengthForTest == 208/*about 3.2KB*/)
  }

  "freeze to high factor keeps length >= 100" in {
    val index = new EventIdPositionIndex(1000)
    for (i ← 1 to 10000) index.addAfter(i, i * 100)
    assert(index.factorForTest == 16 && index.lengthForTest == 626)
    index.freeze(toFactor = 1000)
    assert(index.factorForTest == 96 && index.lengthForTest == 104/*about 1.6KB*/)
  }
}
