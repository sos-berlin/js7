package com.sos.jobscheduler.minicom.remoting.dialog

import akka.util.ByteString
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.Stopwatch
import java.net.InetAddress
import java.util.concurrent.CountDownLatch
import org.scalatest.FreeSpec
import scala.concurrent.ExecutionContext
import scala.language.reflectiveCalls
import scala.util.Random
import scala.util.control.NonFatal

/**
  * @author Joacim Zschimmer
  */
final class LocalServerDialogConnectionExclusiveTest extends FreeSpec {

  InetAddress.getLocalHost  // log4j calls this too. On MacOS this call delays some seconds, so we do it now, before the test.

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
          if (Random.nextInt(5) == 0) Thread.sleep(1)
          msg = con.sendAndReceive(ByteString(msg.get.utf8String + "***")) await 99.s
        }
      }
    }
    val callingThread = new Thread {
      var count = 0
      var throwable: Option[Throwable] = None
      override def run(): Unit =
        try {
          latch.countDown()
          latch.await()
          for (i ← 1 to n) {
            val msg = (con.sendAndReceive(ByteString(s"+$i")) await 99.s).get
            assert(msg == ByteString(s"-+$i"))
            if (Random.nextInt(5) == 0) Thread.sleep(1)
            count += 1
          }
        } catch {
          case _: IllegalStateException ⇒  // Normal end
          case NonFatal(t) ⇒ throwable = Some(t)
        }
    }
    mainThread.start()
    callingThread.start()
    for (i ← 1 to n) {
      var response = (con.leftSendAndReceive(ByteString(s"$i")) await 99.s).utf8String
      Thread.sleep(20)
      while (response startsWith "+") {
        if (Random.nextInt(5) == 0) Thread.sleep(1)
        response = (con.leftSendAndReceive(ByteString(s"-$response")) await 99.s).utf8String
      }
      assert(response == s"$i***")
    }
    info(s"count=${callingThread.count}")
    assert(callingThread.count >= 2)  // For some reason, the nested thread may starve to death before reaching the number.
    for (t ← callingThread.throwable) fail(t)
    con.leftClose()
    mainThread.join(10*1000)
    callingThread.join(10*1000)
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
