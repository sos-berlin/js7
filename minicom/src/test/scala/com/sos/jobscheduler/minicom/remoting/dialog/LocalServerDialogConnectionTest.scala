package com.sos.jobscheduler.minicom.remoting.dialog

import akka.util.ByteString
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.Stopwatch
import java.util.concurrent.CountDownLatch
import org.scalatest.FreeSpec
import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
final class LocalServerDialogConnectionTest extends FreeSpec {

  "LocalServerDialogConnection" in {
    val n = 50
    val latch = new CountDownLatch(2)
    val con = new LocalServerDialogConnection()(ExecutionContext.global)
    val mainThread = new Thread {
      override def run() = {
        var msg = con.receiveFirstMessage() await 99.s
        latch.countDown()
        latch.await()
        while (msg.isDefined) {
          msg = con.sendAndReceive(ByteString(msg.get.utf8String + "***")) await 99.s
        }
      }
    }
    val nestedThread = new Thread {
      var count = 0
      var throwable: Option[Throwable] = None
      override def run(): Unit =
        try {
          latch.countDown()
          latch.await()
          for (i ← 1 to n) {
            val msg = (con.sendAndReceive(ByteString(s"+$i")) await 99.s).get
            assert(msg == ByteString(s"-+$i"))
            count += 1
          }
        } catch {
          case _: IllegalStateException ⇒  // Normal end
          case NonFatal(t) ⇒ throwable = Some(t)
        }
    }
    mainThread.start()
    nestedThread.start()
    for (i ← 1 to n) {
      var response = (con.leftSendAndReceive(ByteString(s"$i")) await 99.s).utf8String
      Thread.sleep(20)
      while (response startsWith "+") {
        response = (con.leftSendAndReceive(ByteString(s"-$response")) await 99.s).utf8String
      }
      assert(response == s"$i***")
    }
    assert(nestedThread.count >= 2)  // For some reason, the nested thread may starve to death before reaching the number.
    for (t ← nestedThread.throwable) fail(t)
    con.leftClose()
    mainThread.join(10*1000)
    nestedThread.join(10*1000)
    assert(!mainThread.isAlive)
  }

  if (sys.props contains "test.speed") "Speed test" in {
    val n = 10000
    val stopwatch = new Stopwatch
    val con = new LocalServerDialogConnection()(ExecutionContext.global)
    val thread = new Thread {
      override def run() = {
        var msg = con.receiveFirstMessage()  await 99.s
        while (msg.isDefined) {
          msg = con.sendAndReceive(ByteString(msg.get.utf8String + "***")) await 99.s
        }
      }
    }
    thread.start()
    for (i ← 1 to n) {
      val response = (con.leftSendAndReceive(ByteString(s"$i")) await 99.s).utf8String
      assert(response == s"$i***")
    }
    con.leftClose()
    println(stopwatch.itemsPerSecondString(n, "sendAndReceive"))
  }
}
