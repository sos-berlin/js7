package com.sos.jobscheduler.agent.scheduler.order

import com.sos.jobscheduler.agent.scheduler.order.BufferedStringWriterTest._
import com.sos.jobscheduler.base.generic.Accepted
import java.io.IOException
import monix.execution.Scheduler
import org.scalatest.FreeSpec
import scala.collection.mutable
import scala.concurrent.Future

/**
  * @author Joacim Zschimmer
  */
final class BufferedStringWriterTest extends FreeSpec {

  private val w = new W

  "Empty at start" in {
    assert(w.isEmpty)
    assert(w.reservedSize == 0)
  }

  "First small chunk is buffered" in {
    w.write("One ")
    assert(w.result.toList == List("onBufferingStarted"))
    assert(!w.isEmpty)
    assert(w.reservedSize == 30)
  }

  "Second small chunk is buffered" in {
    w.write("Two ")
    assert(w.result.toList == List("onBufferingStarted"))
    assert(!w.isEmpty)
    assert(w.reservedSize == 30)
  }

  "Buffer overflow with big chunk releases buffer" in {
    w.write("....'....1....'....2....'")
    assert(w.result.toList == List("onBufferingStarted", "One Two ", "....'....1....'....2....'"))
    assert(w.isEmpty)
    assert(w.reservedSize == 0)
  }

  "Big chunk is passed-through" in {
    w.write("....'....1....")
    assert(w.result.toList == List("onBufferingStarted", "One Two ", "....'....1....'....2....'", "....'....1...."))
    assert(w.isEmpty)
    assert(w.reservedSize == 0)
  }

  "Three small chunks are buffered" in {
    for (_ <- 1 to 3) {
      w.write("123456789")
    }
    assert(w.result.toList == List("onBufferingStarted", "One Two ", "....'....1....'....2....'", "....'....1....", "onBufferingStarted"))
    assert(!w.isEmpty)
    assert(w.reservedSize == 30)
  }

  "Chunk filling the remaining buffer" in {
    w.write("abc")
    assert(w.result.toList == List("onBufferingStarted", "One Two ", "....'....1....'....2....'", "....'....1....", "onBufferingStarted", "123456789123456789123456789abc"))
    assert(w.isEmpty)
    assert(w.reservedSize == 30)
  }

  "flush releases the buffer" in {
    w.flush()
    assert(w.reservedSize == 0)
  }

  "close" in {
    w.write('1')
    w.close()
    intercept[IOException] {
      w.write('2')
    }
  }
}

object BufferedStringWriterTest {
  private class W extends BufferedStringWriter {
    protected val size = 30
    protected val passThroughSize = 10
    protected def scheduler = Scheduler.global
    val result = mutable.Buffer[String]()

    protected def onBufferingStarted() = {
      result += "onBufferingStarted"
    }

    protected def onFlush(string: String) = {
      result += string
      Future.successful(Accepted)
    }
  }
}
