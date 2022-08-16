package js7.journal.watch

import js7.base.test.Test
import js7.common.jsonseq.PositionAnd
import org.scalatest.matchers.should.Matchers.*

/**
  * @author Joacim Zschimmer
  */
final class JournalIndexTest extends Test {

  private val index = new JournalIndex(PositionAnd(100, 1), size = 6)

  "No overflow" in {
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
    for (i <- 13 to 23) index.addAfter(i, i * 100)
    assert(index.positionAndEventIds == Vector(
      PositionAnd( 100,  1),
      PositionAnd( 500,  5),
      PositionAnd( 800,  8),
      PositionAnd(1200, 12),
      PositionAnd(1600, 16),
      PositionAnd(2000, 20)))
  }

  "Overflow #3 with n=2" in {
    for (i <- 24 to 47 by 2) index.addAfter(i, i * 100, n = 2)
    assert(index.positionAndEventIds == Vector(
      PositionAnd( 100,  1),
      PositionAnd( 800,  8),
      PositionAnd(1600, 16),
      PositionAnd(2400, 24),
      PositionAnd(3200, 32),
      PositionAnd(4000, 40)))
  }

  "Positions are evenly distributed after many more positions have been added" in {
    for (i <- 48 to 6000) index.addAfter(i, i * 100)
    assert(index.positionAndEventIds == Vector(
      PositionAnd(   100,  1),
      PositionAnd(102400, 1024),
      PositionAnd(204800, 2048),
      PositionAnd(307200, 3072),
      PositionAnd(409600, 4096),
      PositionAnd(512000, 5120)))
    assert(index.spreadForTest == 1024)
  }

  "A million positions added" in {
    for (i <- 6001 to 1000000) index.addAfter(i, i * 100)
    assert(index.positionAndEventIds == Vector(
      PositionAnd(     100,  1),
      PositionAnd(26214400, 262144),
      PositionAnd(52428800, 524288),
      PositionAnd(78643200, 786432)))
    assert(index.spreadForTest == 262144)
  }

  "freeze" in {
    assert(index.spreadForTest == 262144)
    index.freeze(toFactor = 100)
    index.positionAndEventIds == Vector(
      PositionAnd(     100,  1),
      PositionAnd(26214400, 262144),
      PositionAnd(52428800, 524288),
      PositionAnd(78643200, 786432))
    intercept[IllegalStateException] {
      index.addAfter(99999900, 999)
    }
  }

  "addAfter with known value when freezed" in {
    index.tryAddAfter(1000000, 1000000*100) shouldBe false  // Ignored
    intercept[IllegalStateException] {
      index.tryAddAfter(1000001, 1000001*100)
    }.getMessage shouldEqual "JournalIndex: tryAddAfter(1000001) after freeze 1000000 ?"
  }

  "freeze 2" in {
    val index = new JournalIndex(PositionAnd(100, 1), size = 1000)
    for (i <- 2 to 10000) index.addAfter(i, i * 100)
    assert(index.spreadForTest == 16 && index.lengthForTest == 626)
    index.freeze(toFactor = 50)
    assert(index.spreadForTest == 48 && index.lengthForTest == 208/*about 3.2KB*/)
  }

  "freeze to high factor keeps length >= 100" in {
    val index = new JournalIndex(PositionAnd(100, 1), size = 1000)
    for (i <- 2 to 10000) index.addAfter(i, i * 100)
    assert(index.spreadForTest == 16 && index.lengthForTest == 626)
    index.freeze(toFactor = 1000)
    assert(index.spreadForTest == 96 && index.lengthForTest == 104/*about 1.6KB*/)
  }
}
