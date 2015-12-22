package com.sos.scheduler.engine.common.time.timer

import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.timer.OrderedQueueTest._
import java.time.Instant
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import scala.util.Random

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class OrderedQueueTest extends FreeSpec {

  "Operations" in {
    val queue = newOrderedQueue()
    assert(queue.isEmpty)
    assert(queue.size == 0)
    assert(queue.headOption.isEmpty)
    assert(queue.lastOption.isEmpty)
    intercept[NoSuchElementException] { queue.head }
    val orderedThings = List(Thing(100, "A"), Thing(200, "B"), Thing(200, "C"), Thing(300, "D"))
    queue.add(orderedThings(1))
    queue.add(orderedThings(0))
    queue.add(orderedThings(3))
    queue.add(orderedThings(2))
    assert(!queue.isEmpty)
    assert(queue.size == 4)
    assert(queue.head == orderedThings.head)
    assert(queue.headOption == orderedThings.headOption)
    assert(queue.lastOption == orderedThings.lastOption)
    assert(queue.toSeq == orderedThings)
    assert(queue.popNext(0) == Left(100))
    assert(queue.size == 4)
    assert(queue.popNext(100) == Right(orderedThings(0)))
    assert(queue.size == 3)
    assert(queue.popNext(100) == Left(200))
    assert(queue.popNext(200) == Right(orderedThings(1)))
    assert(queue.popNext(200) == Right(orderedThings(2)))
    assert(queue.popNext(200) == Left(300))
    assert(queue.popNext(300) == Right(orderedThings(3)))
    assert(queue.isEmpty)
    assert(queue.size == 0)
    intercept[NoSuchElementException] { queue.popNext(300) }
  }

  "remove" in {
    val queue = newOrderedQueue()
    val orderedThings = List(
      Thing(100, "100-A"), Thing(100, "100-B"),
      Thing(200, "200-A"),
      Thing(300, "300-A"), Thing(300, "300-B"), Thing(300, "300-C"))
    orderedThings foreach queue.add
    assert(orderedThings.toSeq == orderedThings)
    assert(!queue.remove(100, Thing(200, "200-A")))
    assert(!queue.remove(7, Thing(200, "200-A")))
    assert(!queue.remove(100, Thing(7, "200-A")))
    assert(queue.remove(200, Thing(200, "200-A")))
    assert(queue.toSeq == List(
      Thing(100, "100-A"), Thing(100, "100-B"),
      Thing(300, "300-A"), Thing(300, "300-B"), Thing(300, "300-C")))
    assert(queue.remove(300, Thing(300, "300-C")))
    assert(queue.toSeq == List(
      Thing(100, "100-A"), Thing(100, "100-B"),
      Thing(300, "300-A"), Thing(300, "300-B")))
    assert(queue.remove(300, Thing(300, "300-A")))
    assert(queue.remove(300, Thing(300, "300-B")))
    assert(queue.toSeq == List(Thing(100, "100-A"), Thing(100, "100-B")))
    assert(queue.remove(100, Thing(100, "100-A")))
    assert(queue.toSeq == List(Thing(100, "100-B")))
    assert(queue.remove(100, Thing(100, "100-B")))
    assert(queue.toSeq == Nil)
    assert(queue.isEmpty)
  }

  "Random values, mixed added and popped" in {
    implicit val thingOrdering = new Ordering[Thing] {
      def compare(a: Thing, b: Thing) = a.criterion compare b.criterion
    }
    for (_ ← 1 to 100) {
      val queue = newOrderedQueue()
      val n = 10

      val aLimit = 10 * n
      val aRandoms = Vector.fill(n) { Random.nextInt(aLimit) }
      val aThings = for (i ← aRandoms) yield Thing(i, s"$i")
      aThings foreach queue.add

      def isA(o: Thing) = o.criterion <= aLimit

      assert((queue.toSeq map { _.criterion }).toVector == aRandoms.sorted)
      for (expected ← (aThings filter isA).sorted) {
        val popped = queue.popNext(untilIncluding = aLimit).right.get
        assert(popped == expected)
      }

      val bLimit = 2 * aLimit
      val bRandoms = Vector.fill(n) { aLimit + 1 + Random.nextInt(bLimit - aLimit - 1) }
      val bThings = for (i ← bRandoms) yield Thing(i, s"$i")
      bThings foreach queue.add

      for (expected ← (aThings ++ bThings filterNot isA).sorted) {
        val popped = queue.popNext(untilIncluding = bLimit).right.get
        assert(popped == expected)
      }
    }

    val queue = newOrderedQueue()
    val m = 1000
    val n = 1000
    val randoms = Vector.fill(n) { Random.nextInt(n / 10) }  // Most values occurs multiple times
    val things = for (i ← randoms) yield Thing(i, s"$i")
    val t = Instant.now()
    for (_ <- 1 to m) {
      things foreach queue.add
      for (_ ← 1 to things.size) queue.popNext(untilIncluding = Int.MaxValue)
      assert(queue.isEmpty)
    }
    val duration = Instant.now() - t
    logger.info(s"${duration.pretty} ${1000L*m*n / duration.toMillis}/s")
  }
}

object OrderedQueueTest {
  private val logger = Logger(getClass)

  private def newOrderedQueue(): OrderedQueue[java.lang.Integer, Thing] =
    new TreeMapOrderedQueue((_: Thing).criterion: java.lang.Integer)

  private case class Thing(criterion: Int, string: String)
}
