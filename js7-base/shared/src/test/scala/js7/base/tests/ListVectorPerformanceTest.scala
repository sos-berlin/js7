package js7.base.tests

import js7.base.log.Logger
import js7.base.test.OurTestSuite
import js7.base.tests.ListVectorPerformanceTest.*
import js7.base.time.ScalaTime.RichDeadline
import js7.base.time.Stopwatch.itemsPerSecondString
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.Deadline.now

final class ListVectorPerformanceTest extends OurTestSuite
{
  if (sys.props.contains("test.speed")) {
    "Vector" in {
      for (_ <- 1 to 10) {
        testVector(10, 1_000_000)
      }
      for (_ <- 1 to 10) {
        testVector(100_000, 10)
      }
    }

    "List" in {
      for (_ <- 1 to 10) {
        testList(10, 1_000_000)
      }
      for (_ <- 1 to 10) {
        testList(100_000, 10)
      }
    }

    "Vector again" in {
      for (_ <- 1 to 10) {
        testVector(5, 1_000_000)
      }
    }
  }

  private def testVector(m: Int, n: Int): Unit = {
    val t = now
    for (_ <- 1 to m) {
      val buffer = Vector.newBuilder[String]
      for (_ <- 1 to n) buffer += ""
      val vector = buffer.result()
      assert(vector.head == "")
    }
    logger.info(itemsPerSecondString(t.elapsed, m * n))
  }

  private def testList(m: Int, n: Int): Unit = {
    val t = now
    for (_ <- 1 to m) {
      val buffer = ListBuffer.empty[String]
      for (_ <- 1 to n) buffer += ""
      val list = buffer.toList
      assert(list.head == "")
    }
    logger.info(itemsPerSecondString(t.elapsed, m * n))
  }
}

object ListVectorPerformanceTest {
  private val logger = Logger[this.type]
}
